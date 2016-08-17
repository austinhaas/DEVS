(ns demo.collision-detector-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq? pprint-ev*]]
   [devs.atomic-simulator :refer [atomic-simulator]]
   [devs.immediate-system :refer [immediate-system]]
   [demo.collision-detector :refer [collision-detector]]))

(deftest collision-detector-test
 (is (eq? (immediate-system
           (atomic-simulator (collision-detector 1))
           0
           4
           [[0 [:add [:a 0 0 1]]]
            [0 [:add [:b 0 2 1]]]])
          [[0 [:coll-start {:b {:prev nil, :next [0 1]}, :a {:prev nil, :next [0 1]}}]]
           [1 [:coll-end   {:b {:prev [0 1], :next [2 1]}, :a {:prev [0 1], :next [0 1]}}]]]))

  (is (eq? (immediate-system
            (atomic-simulator (collision-detector 1))
            0
            10
            [[0 [:add [:a 0 0 1]]]
             [0 [:add [:b 5 -2 1]]]])
           [[1.5 [:coll-start {:b {:prev [3 1], :next [1 1]}, :a {:prev [0 1], :next [0 1]}}]]
            [3.5 [:coll-end   {:b {:prev [-1 1], :next [-3 1]}, :a {:prev [0 1], :next [0 1]}}]]])))
