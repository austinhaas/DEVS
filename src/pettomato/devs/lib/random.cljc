(ns pettomato.devs.lib.random
  "Seedable drop-in replacements for Clojure's random functions. Uses
  clojure.test.check.random for the PRNG implementation.

  Be careful with lazy sequences. For example,

  (with-random-seed 123
    (repeatedly 100 rand))

  will not produce repeatable values, because `rand` will be evaluated
  outside `with-random-seed`.

  If the whole seq can be realized, then one solution is to add a
  `doall`. For example,

  (with-random-seed 123
    (doall (repeatedly 100 rand)))

  will produce repeatable values.

  Note that this problem won't occur if the lazy seq is created and
  consumed within the body of `with-random-seed`. An entire program
  could be wrapped in `with-random-seed`. But it is probably a good
  idea to avoid mixing laziness with random number generation (or,
  side-effects, in general). You wouldn't want the sequence of random
  numbers to depend on the lazy sequence chunking implementation, for
  example.

  I assume similar considerations apply to distributed computing, but
  I haven't investigated that. I'd start with the underlying library
  this code is based on: clojure.test.check.random."
  (:refer-clojure :exclude [rand rand-int shuffle random-sample rand-nth])
  #?(:cljs
     (:require-macros
      [pettomato.devs.lib.random :refer [with-random-seed]]))
  (:require
   [clojure.test.check.random :refer [make-random rand-double split]]))

(defn generator
  "Returns a new generator. An optional integer n may be provided as a
  seed. Users should prefer `with-random-seed` to creating a generator
  explicitly."
  ([]  (atom (make-random)))
  ([n] (atom (make-random n))))

(def ^:dynamic *rng*
  "A dynamic variable that can be bound to a generator. Users should
  prefer `with-random-seed` to binding this explicitly."
  (generator))

(defmacro with-random-seed
  "A macro that evaluates body in a context where the global random
  number generator has been seeded with integer n."
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
  "Return a random element of the (sequential) collection. Will have the
  same performance characteristics as nth for the given collection."
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
  "Returns a lazy sequence of the items in coll with random probability
  given by `prob`, which is a number from 0.0 to 1.0, inclusive.
  Returns a transducer when no collection is provided. The items in
  the result will be in the same order as the input collection.

  For example, (random-sample 0.5 xs) will return a collection roughly
  half the size of xs. Each item in xs will have a 50% chance of being
  included in the result.

  See the note in the namespace docstring about laziness."
  ([prob]
   (filter (fn [_] (< (rand) prob))))
  ([prob coll]
   (filter (fn [_] (< (rand) prob)) coll)))
