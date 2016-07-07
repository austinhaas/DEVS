(ns demo.collision-detector-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq? pprint-ev*]]
   [devs.atomic-simulator :refer [atomic-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [demo.integrator :refer [integrator]]
   [demo.collision-detector :refer [collision-detector]]))

(deftest collision-detector-test
 (is (eq? (fast-as-possible-system
           (atomic-simulator (collision-detector 1))
           0
           4
           [[0 [:add [:a 0 0 1]]]
            [0 [:add [:b 0 2 1]]]])
          [[0 [:coll-start {:a [0 1] :b [0 1]}]]
           [1 [:coll-end   {:a [0 1] :b [2 1]}]]]))

  (is (eq? (fast-as-possible-system
            (atomic-simulator (collision-detector 1))
            0
            10
            [[0 [:add [:a 0 0 1]]]
             [0 [:add [:b 5 -2 1]]]])
           [[1.5 [:coll-start {:a [0 1] :b [2 1]}]]
            [3.5 [:coll-end   {:a [0 1] :b [-2 1]}]]])))
