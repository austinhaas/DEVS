(ns test-util
  (:require
   [clojure.walk :refer [postwalk]]))

(defn eq? [a b]
  (let [normalize (partial postwalk (fn [x] (if (number? x) (float x) x)))]
    (= (-> a normalize sort)
       (-> b normalize sort))))

(defn pprint-ev* [ev*]
  (doseq [[t ev] ev*] (println (format "[%.3f] %s" (float t) ev))))
