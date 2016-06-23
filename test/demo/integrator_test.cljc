(ns demo.integrator-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq?]]
   [devs.atomic-simulator :refer [atomic-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [demo.integrator :refer [integrator]]))

(deftest integrator-tests
  (is (eq? (fast-as-possible-system (atomic-simulator (integrator :a 0 1 100 0))
                                    0
                                    400
                                    [])
           [[0   [:pos [:a 0]]]
            [100 [:pos [:a 100]]]
            [200 [:pos [:a 200]]]
            [300 [:pos [:a 300]]]]))

  (is (eq? (fast-as-possible-system (atomic-simulator (integrator :a 0 1 100 0))
                                    0
                                    400
                                    [[100 [:vel 2]]])
           [[0   [:pos [:a 0]]]
            [100 [:pos [:a 100]]]
            [200 [:pos [:a 300]]]
            [300 [:pos [:a 500]]]]))

  (is (eq? (fast-as-possible-system (atomic-simulator (integrator :a 0 1 100 0))
                                    0
                                    500
                                    [[120 [:vel -1]]])
           [[0   [:pos [:a 0]]]
            [100 [:pos [:a 100]]]
            [200 [:pos [:a 40]]]
            [300 [:pos [:a -60]]]
            [400 [:pos [:a -160]]]])))
