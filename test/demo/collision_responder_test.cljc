(ns pettomato.demo.collision-responder-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq?]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.immediate-system :refer [immediate-system]]
   [pettomato.demo.collision-responder :refer [collision-responder]]))

(deftest collision-responder-test
  (is (eq? (immediate-system (atomic-simulator (collision-responder))
                                    0
                                    400
                                    [[0 [:vel [:a 1]]]
                                     [0 [:vel [:b 0]]]
                                     [0 [:coll-start {:a [0 1] :b [0 0]}]]])
           [[0 [:vel [:a -1]]]
            [0 [:vel [:b 0]]]])))
