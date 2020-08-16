(ns pettomato.devs.priority-queue
  "A priority queue implementation.

   Keys are sorted by compare.

   All values with the highest priority are returned as a set.

   Because values with the same key are stored in a set:
     - There is no order between values with the same key.
     - Duplicate key-val pairs are ignored.

  This implementation is intended to be used in cases where delete and
  update are required; and the range of current keys is relatively
  small, so many values map to the same key."
  (:refer-clojure :exclude [empty? peek pop])
  (:require
   [pettomato.devs.util :refer [infinity group]]))

(declare insert)

(defn init
  "Returns a new priority queue with supplied values."
  ([] (sorted-map))
  ([& keyvals] (reduce (fn [m [k v]] (insert m k v)) (sorted-map) (partition 2 keyvals))))

(defn empty?
  "Returns true if pq has no items."
  [pq]
  (clojure.core/empty? pq))

(defn insert
  "Add v to pq, with priority k, unless k is nil or infinity."
  [pq k v]
  (if (or (nil? k) (= k infinity))
    pq
    (update pq k (fnil conj #{}) v)))

(defn insert*
  "Add each item in coll vs to pq with priority k, unless k is nil or infinity."
  [pq k vs]
  (if (or (nil? k) (= k infinity))
    pq
    (update pq k (fnil into #{}) vs)))

(defn delete
  "Delete item v with priority k from pq."
  [pq k v]
  (let [s  (get pq k)
        s' (disj s v)]
    ;; Empty sets are pruned, so the size doesn't grow if the set of possible
    ;; keys is unbound.
    (if (clojure.core/empty? s')
      (dissoc pq k)
      (assoc pq k s'))))

(defn delete*
  "Delete each item in coll vs with priority k from pq."
  [pq k vs]
  (let [s  (get pq k)
        s' (reduce disj s vs)]
    ;; Empty sets are pruned, so the size doesn't grow if the set of possible
    ;; keys is unbound.
    (if (clojure.core/empty? s')
      (dissoc pq k)
      (assoc pq k s'))))

(defn change-priority
  "Change the priority of item v from k1 to k2."
  [pq k1 v k2]
  (if (= k1 k2)
    pq
    (-> pq (delete k1 v) (insert k2 v))))

(defn change-priority*
  "Change the priority of each [k1 v k2] in k1-v-k2*."
  [pq k1-v-k2*]
  (let [[ds is] (reduce (fn [[ds is] [k1 v k2]]
                          [(update ds k1 conj v)
                           (update is k2 conj v)])
                        [{} {}]
                        k1-v-k2*)]
    (as-> pq pq
      (reduce-kv delete* pq ds)
      (reduce-kv insert* pq is))))

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
