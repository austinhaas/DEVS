(ns pettomato.devs.util)

(def infinity #?(:clj  Double/POSITIVE_INFINITY
                 :cljs (.-POSITIVE_INFINITY js/Number)))

(defn group
  "Returns a map of the elements of coll keyed by the result of key-fn
  on each element. The value at each key will be a data structure of
  type val-init, and the elements will be the result of applying
  val-fn to each element, in the order they appeared in coll."
  [key-fn val-fn val-init coll]
  (persistent!
   (reduce
    (fn [m x]
      (let [k (key-fn x)]
        (assoc! m k (conj (get m k val-init) (val-fn x)))))
    (transient {})
    coll)))

(defn group-cons
  "Like group, but val-init is assumed to be a list and values are
  consed onto it."
  [key-fn val-fn coll]
  (persistent!
   (reduce
    (fn [m x]
      (let [k (key-fn x)]
        (assoc! m k (cons (val-fn x) (get m k)))))
    (transient {})
    coll)))

;; https://github.com/weavejester/medley/blob/0.8.1/src/medley/core.cljc#L11
(defn dissoc-in
  "Dissociate a value in a nested assocative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  [m ks]
  (if-let [[k & ks] (seq ks)]
    (if (seq ks)
      (let [v (dissoc-in (get m k) ks)]
        (if (empty? v)
          (dissoc m k)
          (assoc m k v)))
      (dissoc m k))
    m))
