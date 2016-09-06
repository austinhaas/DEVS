(ns pettomato.devs.test-util
  (:require
   [clojure.walk :refer [postwalk]]))

(defn aggregate-timestamped-msg* [tmsg*]
  (reduce (fn [acc [t msg]]
            (if (empty? acc)
              (conj acc [t #{msg}])
              (let [[t' s] (peek acc)]
                (if (= t t')
                  (conj (pop acc) [t (conj s msg)])
                  (conj acc [t #{msg}])))))
          []
          tmsg*))

(defn eq? [a b]
  (let [normalize (partial postwalk (fn [x] (if (number? x) (float x) x)))]
    (= (-> a normalize aggregate-timestamped-msg*)
       (-> b normalize aggregate-timestamped-msg*))))

(defn pprint-timestamped-msg* [tmsg*]
  (doseq [[t msg] tmsg*] (println (format "[%.3f] %s" (float t) msg))))
