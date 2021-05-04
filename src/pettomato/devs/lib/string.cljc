(ns pettomato.devs.lib.string
  #?(:cljs
     (:require
      [goog.string :as gstr]
      [goog.string.format])))

(defn pad-left
  "n - min string length of result
   c - char to add to the left
   s - string to add to"
  [n c s]
  (assert (char? c))
  (if (< (count s) n)
    (recur n c (str c s))
    s))

(defn pad-right
  "n - min string length of result
   c - char to add to the right
   s - string to add to"
  [n c s]
  (assert (char? c))
  (if (< (count s) n)
    (recur n c (str s c))
    s))

(defn format-str [fmt & args]
  #?(:clj  (apply format fmt args)
     :cljs (apply gstr/format fmt args)))
