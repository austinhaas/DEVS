(ns pettomato.devs.lib.hyperreal
  "An implementation of hyperreal numbers.

  The hyperreal numbers are the real numbers extended with infinitesimal (ε) and
  infinity.

  Hyperreal numbers are represented as records with three fields:

  - `infinity` is an integer coefficient indicating the number of infinity
  components in the number

  - `standard` is a real number

  - `infinitesimal` is an integer coefficient indicating the number of
  infinitesimal components in the number

  For example, (*R 1 2 3) = 1∞+2+3ε, and (*R 0 5 0) = 5.

  (*R 0 0 1) is the smallest positive hyperreal number. There is no largest
  hyperreal number; infinity is taken to be an unknown value that is larger than
  all real numbers, but is less than infinity + 1 and infinity * 2.

  Hyperreal numbers may be printed as *ℝ<infinity standard infinitesimal>.

  The component number types are not enforced, but users should respect
  them. Importantly, do not use special numeric values (other than what is
  provided here) like Double/POSITIVE_INFINITY and (.-POSITIVE_INFINITY
  js/Number), because that will produce erroneous results and it would
  unjustifiably complicate the code to support it.

  Barros, Fernando J. \"On the representation of time in modeling & simulation.\" 2016 winter simulation conference (WSC). IEEE, 2016.
  http://simulation.su/uploads/files/default/2016-barros-1.pdf"
  (:refer-clojure :exclude [= + - < <= comparator min max pos?])
  (:require
   [clojure.core :as clj]))

(defrecord Hyperreal [infinity standard infinitesimal]
  Object
  (toString [this] (str "*ℝ<" infinity " " standard " " infinitesimal ">")))

#?(:clj
   (defmethod print-method Hyperreal [h, w]
     (.write w (format "*ℝ<%s %s %s>" (.infinity h) (.standard h) (.infinitesimal h)))))

(defn *R
  "Construct a hyperreal number from its components."
  ([a]     (Hyperreal. 0 a 0))
  ([a z]   (Hyperreal. 0 a z))
  ([i a z] (Hyperreal. i a z)))

(defn standard [x] (:standard x))

(def zero     (*R 0 0 0))
(def epsilon  (*R 0 0 1))
(def infinity (*R 1 0 0))

(defn hyperreal? [x] (instance? Hyperreal x))

(defn =
  ([x] true)
  ([x y]
   (and (clj/== (:infinity x) (:infinity y))
        (clj/== (:standard x) (:standard y))
        (clj/== (:infinitesimal x) (:infinitesimal y))))
  ([x y & more]
   (if (= x y)
     (if (next more)
       (recur y (first more) (next more))
       (= y (first more)))
     false)))

(defn +
  ([] zero)
  ([x] x)
  ([x y]
   (*R (clj/+ (:infinity x) (:infinity y))
       (clj/+ (:standard x) (:standard y))
       (clj/+ (:infinitesimal x) (:infinitesimal y))))
  ([x y & more]
   ;; If more was sufficiently long, it might make sense to stop processing as
   ;; soon as any value is infinity. But, I don't think that will be the case in
   ;; this project. In fact, I don't think this arity is even used at all.
   (reduce + (+ x y) more)))

(defn -
  ([x]
   (*R (clj/- (:infinity x))
       (clj/- (:standard x))
       (clj/- (:infinitesimal x))))
  ([x y]
   (*R (clj/- (:infinity x) (:infinity y))
       (clj/- (:standard x) (:standard y))
       (clj/- (:infinitesimal x) (:infinitesimal y))))
  ([x y & more]
   (reduce - (- x y) more)))

(defn <
  ([x] true)
  ([x y]
   (cond
     (clj/< (:infinity x) (:infinity y))           true
     (clj/< (:infinity y) (:infinity x))           false
     (clj/< (:standard x) (:standard y))           true
     (clj/< (:standard y) (:standard x))           false
     (clj/< (:infinitesimal x) (:infinitesimal y)) true
     (clj/< (:infinitesimal y) (:infinitesimal x)) false
     :else                                         false))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defn <=
  ([x] true)
  ([x y]
   (or (= x y)
       (< x y)))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn comparator [x y]
  (cond
    (= x y) 0
    (< x y) -1
    (< y x) 1))

(defn min
  ([x] x)
  ([x y] (if (< x y) x y))
  ([x y & more]
   (reduce min (min x y) more)))

(defn max
  ([x] x)
  ([x y] (if (< x y) y x))
  ([x y & more]
   (reduce max (max x y) more)))

(defn pos? [x]
  (< zero x))

(defn infinite?
  "Returns true if the infinity component of x is greater than 0."
  [x] (clj/pos? (:infinity x)))
