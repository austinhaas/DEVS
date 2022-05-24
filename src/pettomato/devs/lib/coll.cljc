(ns pettomato.devs.lib.coll
  #?(:clj (:import [java.io Writer])))

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
   (defmethod print-method clojure.lang.PersistentQueue [^clojure.lang.PersistentQueue q ^java.io.Writer w]
     (.write w (format "#queue%s" (vec q)))))

;; https://clojure.atlassian.net/browse/CLJ-1451
(defn take-until
  "Returns a lazy sequence of successive items from coll until
  (pred item) returns true, including that item. pred must be
  free of side-effects. Returns a transducer when no collection
  is provided."
  ([pred]
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (if (pred input)
              (ensure-reduced (rf result input))
              (rf result input))))))
  ([pred coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (if (pred (first s))
           (cons (first s) nil)
           (cons (first s) (take-until pred (rest s))))))))
