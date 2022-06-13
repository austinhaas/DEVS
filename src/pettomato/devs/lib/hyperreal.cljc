(ns pettomato.devs.lib.hyperreal
  "An implementation of hyperreal numbers.

  The hyperreal numbers are the real numbers extended with
  infinitesimal and infinity.

  Infinitesimal (ε) is the smallest positive hyperreal number.

  Infinity (ω) represents a hyperreal number larger than any real
  number.

  This library provides the following hyperreal numbers as constants:

  `zero`
  `epsilon`
  `negative-infinity`
  `positive-infinity`
  `infinity`

  A constructor, `*R`, can be used to create all non-infinite
  hyperreal numbers. For example, `(*R 1 2)` creates a hyperreal
  number with 1 standard unit and 2 infinitesimal units.

  Hyperreal numbers may be printed as +∞, -∞, 5, or 5ε2.

  Note that infinitesimal and infinity are treated differently in
  arithmetic: ε+ε=2ε, but ω+x=ω.

  The real numbers are implemented as floating point numbers on the
  host platform.

  Reference:

  Barros, Fernando J. \"On the representation of time in modeling &
  simulation.\" 2016 winter simulation conference (WSC). IEEE, 2016.
  http://simulation.su/uploads/files/default/2016-barros-1.pdf"
  (:refer-clojure :exclude [= + - < <= comparator min max zero? pos? infinite?])
  (:require
   [clojure.core :as clj]
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.print :as print]))

(defrecord Hyperreal [infinity standard infinitesimal]
  Object
  (toString [this]
    (cond
      (clj/pos? infinity)       "+∞"
      (clj/neg? infinity)       "-∞"
      (clj/zero? infinitesimal) (str standard)
      :else                     (str standard "ε" infinitesimal))))

#?(:clj (print/add-print-handlers-clj Hyperreal)
   :cljs (print/add-print-handlers-cljs Hyperreal))

(defn hyperreal? [x] (instance? Hyperreal x))

(def zero              (Hyperreal. 0 0 0))
(def epsilon           (Hyperreal. 0 0 1))
(def infinity          (Hyperreal. 1 0 0))
(def positive-infinity (Hyperreal. 1 0 0))
(def negative-infinity (Hyperreal. -1 0 0))

(defn- make-hyperreal [infinity standard infinitesimal]
  (ex-assert (not (clj/infinite? standard))
             "standard part cannot be infinite")
  (ex-assert (not (clj/infinite? infinitesimal))
             "infinitesimal part cannot be infinite")
  (cond
    (clj/pos? infinity) positive-infinity
    (clj/neg? infinity) negative-infinity
    :else               (Hyperreal. 0 standard infinitesimal)))

(defn *R
  "Construct a hyperreal number from its components."
  ([standard]               (Hyperreal. 0 standard             0))
  ([standard infinitesimal] (Hyperreal. 0 standard infinitesimal)))

(defn standard
  "Returns the closest standard real number to hyperreal number x,
  where real number means floating point number on the host platform."
  [x]
  (cond
    (clj/zero? (:infinity x)) (:standard x)
    (clj/pos?  (:infinity x)) ##Inf
    (clj/neg?  (:infinity x)) ##-Inf))

(defn =
  ([x] true)
  ([x y]
   (and (clj/== (:infinity      x) (:infinity      y))
        (clj/== (:standard      x) (:standard      y))
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
   (make-hyperreal
    (clj/+ (:infinity      x) (:infinity      y))
    (clj/+ (:standard      x) (:standard      y))
    (clj/+ (:infinitesimal x) (:infinitesimal y))))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn -
  ([x]
   (make-hyperreal
    (clj/- (:infinity      x))
    (clj/- (:standard      x))
    (clj/- (:infinitesimal x))))
  ([x y]
   (make-hyperreal
    (clj/- (:infinity      x) (:infinity      y))
    (clj/- (:standard      x) (:standard      y))
    (clj/- (:infinitesimal x) (:infinitesimal y))))
  ([x y & more]
   (reduce - (- x y) more)))

(defn <
  ([x] true)
  ([x y]
   (cond
     (clj/pos? (:infinity x))                      false
     (clj/pos? (:infinity y))                      true
     (clj/neg? (:infinity y))                      false
     (clj/neg? (:infinity x))                      true
     (clj/< (:standard      x) (:standard      y)) true
     (clj/< (:standard      y) (:standard      x)) false
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
    (= x y)  0
    (< x y) -1
    (< y x)  1))

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

(defn zero? [x]
  (= zero x))

(defn pos? [x]
  (< zero x))

(defn infinite?
  "Returns true if the infinity component of x is not 0."
  [x] (not (clj/zero? (:infinity x))))
