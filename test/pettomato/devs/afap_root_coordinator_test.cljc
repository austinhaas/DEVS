(ns pettomato.devs.afap-root-coordinator
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.example-models :refer [generator]]
   [pettomato.devs.afap-root-coordinator :refer [afap-root-coordinator lazy-afap-root-coordinator]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]))

(deftest afap-test
  (is (= (-> (generator 5 10)
             atomic-simulator
             (afap-root-coordinator 0 20))
         (-> (generator 5 10)
             atomic-simulator
             (lazy-afap-root-coordinator 0 20)))))
