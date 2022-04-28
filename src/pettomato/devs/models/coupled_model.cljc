(ns pettomato.devs.models.coupled-model
  (:require
   [clojure.set :as set]))

(defn validate-coupled-model!
  "Throws an exception if model is invalid."
  [model]
  (let [{:keys [models routes]} model]
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

(defprotocol CoupledModel
  (models [this])
  (routes [this]))

(defrecord BasicCoupledModel [models routes]
  CoupledModel
  (models [this] models)
  (routes [this] routes))

(defn coupled-model
  [models routes]
  (let [model (->BasicCoupledModel models routes)]
    (validate-coupled-model! model)
    model))

(defn coupled-model? [x] (satisfies? CoupledModel x))
