(ns demo.collision-network-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq? pprint-ev*]]
   [devs.executive-network-simulator :refer [network-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [demo.collision-network :refer [collision-network]]))

(pprint-ev*
 (fast-as-possible-system
  (network-simulator (collision-network))
  0
  10
  [[0 [[:add :a] [0 1 1]]]
   [0 [[:add :b] [5 0 1]]]]))
