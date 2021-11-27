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

(defn transpose
  "Takes a collection of collections and returns a new collection of
  collections where the elements are the first items in the input
  collection, then the second items, and so on.

  If you think of the input as a collection of rows, then the output
  is a collection of columns.

  This is intended to be analogous to a matrix transpose function.

  Example:

  (transpose [[:a 0] [:b 1] [:c 2]])
  => [[:a :b :c] [0 1 2]]"
  [coll]
  (apply mapv vector coll))
