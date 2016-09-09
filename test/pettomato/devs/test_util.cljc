(ns pettomato.devs.test-util
  (:require
   [clojure.walk :refer [postwalk]]))

(defn normalize [tmsg*]
  (->> tmsg*
       (postwalk (fn [x] (if (number? x) (float x) x)))
       (map (fn [[t msg*]] [t (set msg*)]))))

(defn eq? [a b]
  (= (normalize a) (normalize b)))

(defn pprint-timestamped-msg* [tmsg*]
  (doseq [[t msg] tmsg*] (println (format "[%.3f] %s" (float t) msg))))
