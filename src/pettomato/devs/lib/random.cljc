(ns pettomato.devs.lib.random
  "Seedable drop-in replacements for Clojure's random functions. Uses
  clojure.test.check.random for the PRNG implementation."
  (:refer-clojure :exclude [rand rand-int shuffle random-sample rand-nth])
  #?(:cljs
     (:require-macros
      [pettomato.devs.lib.random :refer [with-random-seed]]))
  (:require
   [clojure.test.check.random :refer [make-random rand-double split]]))

(defn generator
  "Returns a new generator. An optional seed may be provided. Users should prefer
  `with-random-seed` to creating a generator explicitly."
  ([]  (atom (make-random)))
  ([n] (atom (make-random n))))

(def ^:dynamic *rng*
  "A dynamic variable that can be bound to a generator. Users should prefer
  `with-random-seed` to binding this explicitly."
  (generator))

(defmacro with-random-seed
  "A macro that evaluates body in a context where the global random number
  generator has been seeded with integer n."
  [n & body]
  `(binding [*rng* (generator ~n)]
     ~@body))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  ([]  (rand-double (swap! *rng* (comp first split))))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  [n]
  (int (rand n)))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection."
  [coll]
  (nth coll (rand-int (count coll))))

(defn shuffle
  "Return a random permutation of coll.

  Uses the Fisher-Yates shuffle:
  https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle

  Not lazy!"
  [coll]
  (let [a (to-array coll)]
    (loop [i (dec (alength a))]
      (when (< 0 i)
        (let [j   (rand-int (inc i))
              tmp (aget a i)]
          (aset a i (aget a j))
          (aset a j tmp)
          (recur (dec i)))))
    (vec a)))

(defn random-sample
  "Returns items from coll with random probability of prob (0.0 -
  1.0).  Returns a transducer when no collection is provided."
  ([prob]
   (filter (fn [_] (< (rand) prob))))
  ([prob coll]
   (filter (fn [_] (< (rand) prob)) coll)))
