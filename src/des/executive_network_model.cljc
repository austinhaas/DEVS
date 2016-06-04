(ns des.executive-network-model)

(defn executive-network-model [executive-name executive-model]
  {:type            ::network
   :executive-name  executive-name
   :executive-model executive-model})

(defn network? [model] (= ::network (:type model)))
