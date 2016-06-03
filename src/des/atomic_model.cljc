(ns des.atomic-model)

(defn atomic-model [initial-state int-update-fn ext-update-fn con-update-fn output-fn time-advance-fn]
  {:type            ::atomic
   :initial-state   initial-state
   :int-update-fn   int-update-fn
   :ext-update-fn   ext-update-fn
   :con-update-fn   (if (nil? con-update-fn)
                      (fn [s x] (ext-update-fn (int-update-fn s) 0 x))
                      con-update-fn)
   :output-fn       output-fn
   :time-advance-fn time-advance-fn})

(defn atomic? [model] (= ::atomic (:type model)))
