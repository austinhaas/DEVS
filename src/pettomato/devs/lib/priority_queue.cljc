(ns pettomato.devs.lib.priority-queue
  "A priority queue implementation.

  Keys are sorted by `compare`.

  Items with the same priority belong to a set.

  This implementation is intended for cases where efficient delete and update
  are needed, and many values map to the same key.

  This implementaton was motivated by a particular discrete-event simulation,
  where the priority queue's keys are time-of-next-update and the values are
  simulation models. Delete was needed to update entries that had not yet
  reached the top of the queue. Many events mapped to a small number of keys,
  because, for aesthetic reasons, many models shared the same general update
  cycle."
  (:refer-clojure :exclude [empty? peek pop]))

(declare insert)

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
    ;; minimum. This is especially important for cases where new keys are
    ;; constantly being introduced and old keys are never used again, such as
    ;; with keys that represent advancing points in time.
    (if (clojure.core/empty? (get pq k))
      (dissoc pq k)
      pq)))

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
  "Removes ALL items with highest priority."
  [pq]
  (dissoc pq (ffirst pq)))

(defn ->seq
  "Returns the contents of priority-queue pq as an ordered seq."
  [pq]
  (seq pq))
