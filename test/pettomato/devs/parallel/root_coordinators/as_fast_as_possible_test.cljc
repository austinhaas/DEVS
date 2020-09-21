(ns pettomato.devs.parallel.root-coordinators.as-fast-as-possible-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.parallel.models.examples :refer [generator]]
   [pettomato.devs.parallel.root-coordinators.as-fast-as-possible
    :refer [afap-root-coordinator lazy-afap-root-coordinator]]
   [pettomato.devs.parallel.simulators.atomic :refer [atomic-simulator]]))

(deftest afap-test
  (is (= (-> (generator 5 10)
             atomic-simulator
             (afap-root-coordinator 0 20))
         (-> (generator 5 10)
             atomic-simulator
             (lazy-afap-root-coordinator 0 20)))))
