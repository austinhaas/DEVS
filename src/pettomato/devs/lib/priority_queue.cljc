(ns pettomato.devs.lib.priority-queue
  "A priority queue implementation.

  Keys are sorted by `compare`.

  Items with the same priority belong to a set.

  This implementation supports efficient delete and update, and
  assumes that many values map to the same key.

  This implementation was motivated by a particular discrete-event
  simulation system, where simulation models are indexed by the time
  of their next update. Many events map to a small number of keys,
  when simulation models are on the same update cycle."
  (:refer-clojure :exclude [empty empty? into peek pop]))

(defn priority-queue
  "Returns a new priority queue."
  ([]
   (sorted-map))
  ([comparator]
   (sorted-map-by comparator)))

(defn empty?
  "Returns true if pq has no items."
  [pq]
  (clojure.core/empty? pq))

(defn insert
  "Add v to pq, with priority k."
  [pq k v]
  (update pq k (fnil conj #{}) v))

(defn delete
  "Delete item v with priority k from pq."
  [pq k v]
  (let [pq (update pq k disj v)]
    ;; Empty sets are pruned, to keep the total size of the map to a
    ;; minimum. This is especially important for cases where new keys
    ;; are constantly being introduced and old keys are never used
    ;; again, such as with keys that represent advancing points in
    ;; time.
    (if (clojure.core/empty? (get pq k))
      (dissoc pq k)
      pq)))

(defn empty
  [pq]
  (clojure.core/empty pq))

(defn change-priority
  "Change the priority of item v from k1 to k2."
  [pq k1 v k2]
  (if (= k1 k2)
    pq
    (-> pq (delete k1 v) (insert k2 v))))

(defn peek-key
  "Returns the highest priority in pq."
  [pq]
  (ffirst pq))

(defn peek
  "Returns a set containing ALL items with highest priority."
  [pq]
  (second (first pq)))

(defn pop
  "Returns a new priority queue that is equal to pq without the set of
  highest priority items."
  [pq]
  (dissoc pq (ffirst pq)))

(defn into [pq kvs]
  (reduce (fn [pq [k v]] (insert pq k v))
          pq
          kvs))

(defn ->seq
  "Returns the contents of priority-queue pq as an ordered seq."
  [pq]
  (seq pq))
