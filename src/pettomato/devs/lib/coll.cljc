(ns pettomato.devs.lib.coll)

(defn prune
  "Recursively removes empty values in a nested associative structure."
  [m ks]
  (if (seq ks)
    (let [[k & ks] ks
          v        (prune (get m k) ks)]
      (if (seq v)
        (assoc m k v)
        (dissoc m k)))
    m))

(def queue
  "A FIFO queue."
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs cljs.core.PersistentQueue.EMPTY))

#?(:clj
   (defmethod print-method clojure.lang.PersistentQueue [q, w]
     (.write w (format "#queue%s" (vec q)))))

(defn collect-by
  "Example:

  (collect-by (juxt first second) [[:a 0] [:b 1]])
  => ([:a :b] [0 1])"
  [f xs]
  (reduce (fn [acc x]
            (map conj
                 (concat acc (repeat []))
                 (f x)))
          []
          xs))
