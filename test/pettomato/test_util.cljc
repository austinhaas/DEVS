(ns pettomato.test-util
  (:require
   [clojure.walk :refer [postwalk]]))

(defn aggregate-events [events]
  (reduce (fn [acc [t ev]]
            (if (empty? acc)
              (conj acc [t #{ev}])
              (let [[t' s] (peek acc)]
                (if (= t t')
                  (conj (pop acc) [t (conj s ev)])
                  (conj acc [t #{ev}])))))
          []
          events))

(defn eq? [a b]
  (let [normalize (partial postwalk (fn [x] (if (number? x) (float x) x)))]
    (= (-> a normalize aggregate-events)
       (-> b normalize aggregate-events))))

(defn pprint-ev* [ev*]
  (doseq [[t ev] ev*] (println (format "[%.3f] %s" (float t) ev))))
