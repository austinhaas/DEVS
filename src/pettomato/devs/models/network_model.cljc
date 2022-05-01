(ns pettomato.devs.models.network-model
  (:require
   [clojure.set :as set]
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.network-executive-model :refer [executive-model?]]))

(defrecord NetworkModel [executive-id executive-model])

(defn validate-network-model!
  "Throws an exception if model is invalid."
  [model]
  (let [[exec elapsed] (:executive-model model)]
    (assert (executive-model? exec))
    (assert (atomic-model? exec)))
  nil)

(defn network-model
  [network-executive-id network-executive-model]
  (let [model (->NetworkModel network-executive-id network-executive-model)]
    (validate-network-model! model)
    model))

(defn network-model? [x] (instance? NetworkModel x))
