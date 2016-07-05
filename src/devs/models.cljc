(ns devs.models)

(defn atomic-model [initial-state
                    int-update-fn ext-update-fn con-update-fn
                    output-fn time-advance-fn]
  {:type            ::atomic
   :initial-state   initial-state
   :int-update-fn   int-update-fn
   :ext-update-fn   ext-update-fn
   :con-update-fn   (if (nil? con-update-fn)
                      (fn [s e x] (ext-update-fn (int-update-fn s) 0 x))
                      con-update-fn)
   :output-fn       output-fn
   :time-advance-fn time-advance-fn})

(defn executive-model [initial-state
                       int-update-fn ext-update-fn con-update-fn
                       output-fn time-advance-fn]
  (-> (atomic-model initial-state int-update-fn ext-update-fn con-update-fn output-fn time-advance-fn)
      (assoc :type ::executive)))

(defn network-model [executive-name executive-model]
  {:type            ::network
   :executive-name  executive-name
   :executive-model executive-model})

(defn atomic?    [model] (or (= ::atomic (:type model))
                             (= ::executive (:type model))))
(defn executive? [model] (= ::executive (:type model)))
(defn network?   [model] (= ::network (:type model)))
