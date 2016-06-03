(ns des.network-model)

(defn network-model [components connections]
  {:type        ::network
   :components  components
   :connections connections})

(defn network? [model] (= ::network (:type model)))
