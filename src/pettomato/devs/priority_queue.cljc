(ns pettomato.devs.priority-queue
  "A priority queue implementation.

   Keys are sorted by compare.

   All values with the highest priority are returned as a set.

   Because values with the same key are stored in a set:
     - There is no order between values with the same key.
     - Duplicate key-val pairs are ignored.

   This implementation is intended to be used in cases where delete
  and update are required; and the range of current keys is relatively
  small, so many values map to the same key."
  (:refer-clojure :exclude [peek pop])
  (:require [pettomato.devs.util :refer [infinity]]))

(declare insert)

(defn priority-queue
  "Returns a new priority queue with supplied values."
  ([] (sorted-map))
  ([& keyvals] (reduce (fn [m [k v]] (insert m k v)) (sorted-map) (partition 2 keyvals))))

(defn insert
  "Add v to priority-queue, pq, with priority k, unless k is nil or
  infinity."
  [pq k v]
  (if (or (nil? k) (= k infinity))
    pq
    (update pq k (fnil conj #{}) v)))

(defn delete
  "Remove v from priority-queue, pq."
  [pq k v]
  (let [pq' (clojure.core/update pq k disj v)]
    (if (empty? (get pq' k))
      (dissoc pq' k)
      pq')))

(defn modify [pq k1 v k2]
  (if (= k1 k2)
    pq
    (-> pq (delete k1 v) (insert k2 v))))

(defn peek
  "Returns a set containing ALL items with highest priority."
  [pq]
  (second (first pq)))

(defn pop
  "Removes ALL items with highest priority."
  [pq]
  (dissoc pq (ffirst pq)))

(defn peek-key
  "Returns the highest priority in pq."
  [pq]
  (ffirst pq))
