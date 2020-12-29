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
