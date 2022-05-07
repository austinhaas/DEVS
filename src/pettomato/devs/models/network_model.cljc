(ns pettomato.devs.models.network-model
  (:require
   [clojure.set :as set]
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.executive-model :refer [executive-model?]]))

(defrecord NetworkModel [executive-id executive-model models routes])

(defn validate-network-model!
  "Throws an exception if model is invalid."
  [model]
  (let [[executive-model elapsed] (:executive-model model)
        executive-id              (:executive-id model)
        {:keys [models routes]}   model
        models                    (assoc models executive-id executive-model)]
    (assert (executive-model? executive-model))
    (assert (atomic-model? executive-model))
    ;; A coupled model must contain at least one component model.
    (when (zero? (count models))
      (throw (ex-info "A coupled model must contain at least one component model."
                      model)))
    ;; All models in routes must appear in models (except for :network).
    (let [keys-in-routes (-> (mapcat (fn [[sk _ rk _ _]] [sk rk])
                                     routes)
                             set
                             (disj :network))
          keys-in-models (-> models
                             keys
                             set)]
     (when-not (set/subset? keys-in-routes keys-in-models)
       (throw (ex-info (str "All models in routes must appear in models (except for :network). Unknown models in routes: "
                            (set/difference keys-in-routes keys-in-models))
                       model))))
    ;; A model cannot use the same port for both input and output.
    (let [input-ports  (set (for [[sk sp _ _ _] routes] [sk sp]))
          output-ports (set (for [[_ _ rk rp _] routes] [rk rp]))]
      (when (seq (set/intersection input-ports output-ports))
        (throw (ex-info "A model cannot use the same port for both input and output."
                        model))))
    ;; A network input port cannot connect directly to a network output port.
    (let [pass-thru-routes (filter (fn [[sk _ rk _ _]] (= sk rk))
                                   routes)]
      (when (seq pass-thru-routes)
        (throw (ex-info (str "A network input port cannot connect directly to a network output port: "
                             pass-thru-routes)
                        model)))))
  nil)

(defn network-model
  ([executive-id executive-model]
   (network-model executive-id executive-model {} []))
  ([executive-id executive-model models routes]
   (let [model (->NetworkModel executive-id executive-model models routes)]
     (validate-network-model! model)
     model)))

(defn network-model? [x] (instance? NetworkModel x))
