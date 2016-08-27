(ns pettomato.demo.collision-detector-test
  (:require
   [clojure.test :refer :all]
   [pettomato.test-util :refer [eq? pprint-ev*]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.immediate-system :refer [immediate-system]]
   [pettomato.demo.collision-detector :refer [collision-detector]]))


(immediate-system
 (atomic-simulator (collision-detector 1 (constantly true)))
 0
 4
 [[0 [[:sub-response :q]
      [[{} {}]
       #{{:old {}
          :new {:id :a :collision-group :alpha :shape [0 0 5 5] :pos [0 0] :next-pos [2 0]}
          :oldf {}
          :newf {}}
         {:old {}
          :new {:id :b :collision-group :alpha :shape [0 0 5 5] :pos [6 0] :next-pos [6 0]}
          :oldf {}
          :newf {}}}]]]])
