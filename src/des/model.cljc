(ns des.model)

(defn model [initial-state int-update-fn ext-update-fn con-update-fn output-fn time-advance-fn]
  {:type            ::atomic
   :initial-state   initial-state
   :int-update-fn   int-update-fn
   :ext-update-fn   ext-update-fn
   :con-update-fn   (if (nil? con-update-fn)
                      (fn [s x] (ext-update-fn (int-update-fn s) 0 x))
                      con-update-fn)
   :output-fn       output-fn
   :time-advance-fn time-advance-fn})

(defn network [components connections]
  {:type        ::network
   :components  components
   :connections connections})

(defn atomic?  [model] (= ::atomic  (:type model)))
(defn network? [model] (= ::network (:type model)))
