(ns pettomato.devs.dsdevs.models.network)

(defn network-model [exec-name exec-model]
  (assert (executive? exec-model))
  {:type            ::network
   :executive-name  exec-name
   :executive-model exec-model})

(defn exec-name  [model] (:executive-name  model))
(defn exec-model [model] (:executive-model model))

(defn network-model? [model] (= ::network (:type model)))
