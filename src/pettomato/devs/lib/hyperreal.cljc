(ns pettomato.devs.lib.hyperreal
  "An implementation of hyperreal numbers.

  The hyperreal numbers are the real numbers extended with infinitesimal (ε) and
  infinity.

  Hyperreal numbers are represented as either,

    - `infinity`, the symbol, or
    - [a zε], a vector pair, where a ∈ ℝ and z ∈ ℤ. Note that ε is implied in
  actual instances, such as [0 0] [3 1] and [5 -1].

  [5 -1] is one ε before 5.

  Negative infinity is not supported. We don't need it in this project, so
  there's no reason to complicate the code. This means you cannot subtract
  infinity from anything or `(- infinity)`; both will throw an exception.

  The number types are not enforced, but users should respect them. Importantly,
  do not use special numeric values (other than what is provided here) like
  Double/POSITIVE_INFINITY and (.-POSITIVE_INFINITY js/Number), because that
  will produce erroneous results and it would unjustifiably complicate the code
  to support it. Just don't do it. [infinity 300ε] doesn't make sense here.

  Barros, Fernando J. \"On the representation of time in modeling & simulation.\" 2016 winter simulation conference (WSC). IEEE, 2016.
  http://simulation.su/uploads/files/default/2016-barros-1.pdf"
  (:refer-clojure :exclude [= + - < <= comparator min max pos?])
  (:require
   [clojure.core :as clj]))

(defn H
  "Construct a hyperreal number from its components."
  ([a]   [a 0])
  ([a z] [a z]))

(def zero (H 0))

(def epsilon (H 0 1))

(def infinity 'infinity)

(defn infinity? [x] (clj/= x infinity))

(defn hyperreal? [x] (or (and (vector? x)
                              (number? (first x))
                              (int? (second x)))
                         (infinity? x)))

(defn =
  ([x] true)
  ([x y]
   (cond
     (infinity? x) (infinity? y)
     (infinity? y) false
     :else         (let [[a m] x
                         [b n] y]
                     (and (== a b)
                          (== m n)))))
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
   (cond
     (infinity? x) infinity
     (infinity? y) infinity
     :else         (let [[a m] x
                         [b n] y]
                      (H (clj/+ a b)
                         (clj/+ m n)))))
  ([x y & more]
   ;; If more was sufficiently long, it might make sense to stop processing as
   ;; soon as any value is infinity. But, I don't think that will be the case in
   ;; this project. In fact, I don't think this arity is even used at all.
   (reduce + (+ x y) more)))

(defn -
  ([x]
   (if (infinity? x)
     (throw (ex-info "Unsupported operation. You can't negate infinity." {}))
     (let [[a m] x]
       (H (clj/- a) (clj/- m)))))
  ([x y]
   (cond
     (infinity? y) (throw (ex-info "Unsupported operation. You can't subtract infinity." {}))
     (infinity? x) infinity
     :else         (let [[a m] x
                         [b n] y]
                     (H (clj/- a b)
                        (clj/- m n)))))
  ([x y & more]
   (reduce - (- x y) more)))

(defn <
  ([x] true)
  ([x y]
   (cond
     (infinity? x) false
     (infinity? y) true
     :else         (let [[a m] x
                         [b n] y]
                     (or (clj/< a b)
                         (and (== a b)
                              (clj/< m n))))))
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
    (= x y)       0
    (infinity? x) 1
    (infinity? y) -1
    :else         (let [[a m] x
                        [b n] y]
                    (cond
                      (clj/< a b) -1
                      (clj/< b a) 1
                      (clj/< m n) -1
                      (clj/< n m) 1))))

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

(defn standard [x] (let [[a m] x] a))
