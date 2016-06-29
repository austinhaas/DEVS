(ns demo.collision-network-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq? pprint-ev*]]
   [clojure.core.async :as async :refer [chan go <! timeout close! >! onto-chan]]
   [devs.executive-network-simulator :refer [network-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [devs.real-time-system :refer [real-time-system]]
   [demo.collision-network :refer [collision-network]]))
#_
(pprint-ev*
 (fast-as-possible-system
  (network-simulator (collision-network))
  0
  10
  [[0 [[:add :a] [0 1 1]]]
   [0 [[:add :b] [5 -1 1]]]
   #_[0 [[:add :c] [5 0 1]]]]))

#_
(do
  (def sim (network-simulator (collision-network)))
  (def chan-in  (chan 100))
  (def chan-out (chan 100))

  (onto-chan chan-in [[[:add :a] [0 1 1]]
                      [[:add :b] [5 -1 1]]]
             false)

  (real-time-system sim 0 chan-in chan-out)

  (go (loop []
        (if-let [v (<! chan-out)]
          (do (println (format "[%s] %s" (first v) (second v)))
              (recur))
          (println 'done)))))

#_(close! chan-in)
