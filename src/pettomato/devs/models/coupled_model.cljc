(ns pettomato.devs.models.coupled-model)

(defprotocol CoupledModel
  (models [this])
  (routes [this]))

(defrecord BasicCoupledModel [models routes]
  CoupledModel
  (models [this] models)
  (routes [this] routes))

(defn coupled-model
  [models routes]
  (->BasicCoupledModel models routes))

(defn coupled-model? [x] (satisfies? CoupledModel x))
