(ns pettomato.devs.models.network-model
  (:require
   [clojure.set :as set]
   [pettomato.devs.lib.debug :refer [ex-assert]]
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
    (ex-assert (executive-model? executive-model)
               "Executive must be an executive model.")
    (ex-assert (atomic-model? executive-model)
               "Executive must be an atomic model.")
    ;; All models in routes must appear in models (except for :network).
    (let [keys-in-routes (-> (mapcat (fn [[sk _ rk _ _]] [sk rk])
                                     routes)
                             set
                             (disj :network))
          keys-in-models (-> models
                             keys
                             set)]
      (ex-assert (set/subset? keys-in-routes keys-in-models)
                 "All models in routes must appear in models (except for :network)."
                 {:unknown-models (set/difference keys-in-routes keys-in-models)}))
    ;; A model cannot use the same port for both input and output.
    (let [input-ports  (set (for [[sk sp _ _ _] routes] [sk sp]))
          output-ports (set (for [[_ _ rk rp _] routes] [rk rp]))]
      (ex-assert (empty? (set/intersection input-ports output-ports))
                 "A model cannot use the same port for both input and output."))
    ;; A network input port cannot connect directly to a network output port.
    (let [pass-thru-routes (filter (fn [[sk _ rk _ _]] (= sk rk))
                                   routes)]
      (ex-assert (empty? pass-thru-routes)
                 "A network input port cannot connect directly to a network output port."
                 {:bad-routes pass-thru-routes})))
  nil)

(defn network-model
  "Construct a network model. Optionally, an initial set of models and
  routes may be supplied."
  ([executive-id executive-model]
   (network-model executive-id executive-model {} []))
  ([executive-id executive-model models routes]
   (let [model (->NetworkModel executive-id executive-model models routes)]
     (validate-network-model! model)
     model)))

(defn network-model? [x] (instance? NetworkModel x))
