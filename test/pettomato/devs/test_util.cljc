(ns pettomato.devs.test-util
  (:require
   [clojure.walk :refer [postwalk]]))

(defn normalize [tmsg*]
  (->> tmsg*
       (postwalk (fn [x] (if (number? x) (float x) x)))
       (map (fn [[t m]] [t (reduce-kv (fn [m k v]
                                        (assoc m k (set v)))
                                      {}
                                      m)]))))

(defn eq? [a b]
  (= (normalize a) (normalize b)))

(defn pprint-timestamped-msg* [tmsg*]
  (doseq [[t msg] tmsg*] (println (format "[%.3f] %s" (float t) msg))))
