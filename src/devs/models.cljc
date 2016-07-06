(ns devs.models)

(defn atomic?    [model] (or (= ::atomic (:type model))
                             (= ::executive (:type model))))
(defn executive? [model] (= ::executive (:type model)))
(defn network?   [model] (= ::network (:type model)))

(defn atomic-model [init int ext con out ta]
  (assert (or (nil? int) (ifn? int)))
  (assert (or (nil? ext) (ifn? ext)))
  (assert (or (nil? con) (ifn? con)))
  (assert (or (nil? out) (ifn? out)))
  (assert (or (nil? ta)  (ifn? ta)))
  {:type            ::atomic
   :initial-state   init
   :int-update-fn   int
   :ext-update-fn   ext
   :con-update-fn   (if (nil? con)
                      (fn [s e x] (ext (int s) 0 x))
                      con)
   :output-fn       out
   :time-advance-fn ta})

(defn executive-model [init int ext con out ta]
  (assert (contains? init :components))
  (assert (contains? init :connections))
  (-> (atomic-model init int ext con out ta)
      (assoc :type ::executive)))

(defn network-model [exec-name exec-model]
  (assert (executive? exec-model))
  {:type            ::network
   :executive-name  exec-name
   :executive-model exec-model})
