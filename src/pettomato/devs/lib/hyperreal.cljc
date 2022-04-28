(ns pettomato.devs.lib.hyperreal
  "An implementation of hyperreal numbers.

  The hyperreal numbers are the real numbers extended with infinitesimal (ε) and
  infinity (ω).

  Hyperreal numbers are represented as the sum of three components:
  Aω+r+Bε. They are implemented as a record with three fields:

  - `infinity` is an integer coefficient indicating the number of infinity
  units in the number (i.e., A in Aω+r+Bε).

  - `standard` is a real number (i.e., r in Aω+r+Bε).

  - `infinitesimal` is an integer coefficient indicating the number of
  infinitesimal units in the number (i.e., B in Aω+r+Bε).

  For example, with constructor *R, (*R 1 2 3) = 1ω+2+3ε, and (*R 0 5.2 0) =
  0ω+5.2+0ε = 5.2.

  (*R 0 0 1), meaning ε, or `epsilon`, is the smallest positive hyperreal
  number. There is no largest hyperreal number; `infinity` is taken to be an
  unknown quantity that is larger than all real numbers, but less than
  infinity+1 and infinity*2.

  Hyperreal numbers may be printed as *ℝ<infinity standard infinitesimal>.

  The component number types are not enforced, but users should respect
  them. Importantly, do not use special numeric values like
  Double/POSITIVE_INFINITY and (.-POSITIVE_INFINITY js/Number), because that
  will produce erroneous results and it would unjustifiably complicate the code
  to support it.

  Also note that the real number implementation is based on the host platform,
  so the real numbers are represented as floating-point numbers with all their
  associated deficiencies.

  Reference:

  Barros, Fernando J. \"On the representation of time in modeling & simulation.\" 2016 winter simulation conference (WSC). IEEE, 2016.
  http://simulation.su/uploads/files/default/2016-barros-1.pdf"
  (:refer-clojure :exclude [= + - < <= comparator min max zero? pos? infinite?])
  (:require
   [clojure.core :as clj]
   #?(:cljs [goog.string :as gstring :refer [format]])
   #?(:cljs [goog.string.format])))

(defrecord Hyperreal [infinity standard infinitesimal]
  Object
  (toString [this] (str "*ℝ<" infinity " " standard " " infinitesimal ">")))

(defn format-hyperreal [x]
  (format "*ℝ<%s %s %s>" (:infinity x) (:standard x) (:infinitesimal x)))

#?(:clj
   (defn pretty-print-hyperreal [x w]
     (.write w (format-hyperreal x))))

#?(:clj
   (defmethod print-method Hyperreal [x w]
     (pretty-print-hyperreal x w)))

#?(:clj
   (. clojure.pprint/simple-dispatch
      addMethod
      Hyperreal
      #(pretty-print-hyperreal %1 *out*)))

(defn *R
  "Construct a hyperreal number from its components."
  ([standard]                        (Hyperreal.        0 standard             0))
  ([standard infinitesimal]          (Hyperreal.        0 standard infinitesimal))
  ([infinity standard infinitesimal] (Hyperreal. infinity standard infinitesimal)))

(defn hyperreal? [x] (instance? Hyperreal x))

(defn standard
  "Returns the closest standard real number to hyperreal number x,
  where real number means floating point number in the host platform."
  [x]
  (cond
    (clj/zero? (:infinity x)) (:standard x)
    (clj/pos?  (:infinity x)) #?(:clj  Double/POSITIVE_INFINITY
                                 :cljs (.-POSITIVE_INFINITY js/Number))
    (clj/neg?  (:infinity x)) #?(:clj  Double/NEGATIVE_INFINITY
                                 :cljs (.-NEGATIVE_INFINITY js/Number))))

(def zero     (*R 0 0 0))
(def epsilon  (*R 0 0 1))
(def infinity (*R 1 0 0))

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
   (*R (clj/+ (:infinity      x) (:infinity      y))
       (clj/+ (:standard      x) (:standard      y))
       (clj/+ (:infinitesimal x) (:infinitesimal y))))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn -
  ([x]
   (*R (clj/- (:infinity      x))
       (clj/- (:standard      x))
       (clj/- (:infinitesimal x))))
  ([x y]
   (*R (clj/- (:infinity      x) (:infinity      y))
       (clj/- (:standard      x) (:standard      y))
       (clj/- (:infinitesimal x) (:infinitesimal y))))
  ([x y & more]
   (reduce - (- x y) more)))

(defn <
  ([x] true)
  ([x y]
   (cond
     (clj/< (:infinity      x) (:infinity      y)) true
     (clj/< (:infinity      y) (:infinity      x)) false
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
