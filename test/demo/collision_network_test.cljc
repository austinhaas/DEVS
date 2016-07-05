(ns demo.collision-network-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq? pprint-ev*]]
   [pt-lib.coll :refer [dissoc-in]]
   [pt-lib.match :refer [match]]
   [pt-lib.number :refer [infinity]]
   [devs.models :refer [atomic-model executive-model network-model]]
   [demo.integrator :refer [mult-integrator]]
   [demo.collision-detector :refer [collision-detector]]
   [demo.collision-responder :refer [collision-responder]]
   [clojure.core.async :as async :refer [chan go <! timeout close! >! onto-chan]]
   [devs.network-simulator :refer [network-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [devs.real-time-system :refer [real-time-system]]))

(defn collision-network-1 []
  (network-model
   :net
   (executive-model
    {:components  {:int   (mult-integrator 1)
                   :c-det (collision-detector 1)}
     :connections {:N     {:add        {:int   :add
                                        :c-det :add}
                           :rem        {:int   :rem
                                        :c-det :rem}
                           :vel        {:int   :vel
                                        :c-det :vel}}
                   :int   {:pos        {:N   :pos}}
                   :c-det {:coll-start {:N   :coll-start}
                           :coll-end   {:N   :coll-end}}}}
    nil nil nil nil
    (constantly infinity))))

(defn collision-network-2 []
  (network-model
   :net
   (executive-model
    {:components  {:int   (mult-integrator 1)
                   :c-det (collision-detector 1)
                   :c-res (collision-responder)}
     :connections {:N     {:add        {:int   :add
                                        :c-det :add
                                        :c-res :add}
                           :rem        {:int   :rem
                                        :c-det :rem
                                        :c-res :rem}
                           :vel        {:int   :vel
                                        :c-det :vel
                                        :c-res :vel}}
                   :int   {:pos        {:N     :pos}}
                   :c-det {:coll-start {:N     :coll-start
                                        :c-res :coll-start}
                           :coll-end   {:N     :coll-end}}
                   :c-res {:vel        {:int   :vel
                                        :c-det :vel}}}}
    nil nil nil nil
    (constantly infinity))))

#_
(do
  (def sim (network-simulator (collision-network-1)))
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

(deftest collision-network-tests-1
  (testing "2 particles. Both Stationary. Initially intersecting."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              2
              [[0 [:add [:a 0 0 1]]]
               [0 [:add [:b 0 0 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 0]]]
              [0 [:coll-start #{:b :a}]]
              [1 [:pos [:a 0]]]
              [1 [:pos [:b 0]]]])))
  (testing "2 particles. One moving. Initially intersecting."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              3
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 0 0 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 0]]]
              [0 [:coll-start #{:b :a}]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 0]]]
              [2 [:pos [:a 2]]]
              [2 [:pos [:b 0]]]
              [2 [:coll-end #{:b :a}]]])))
  (testing "2 particles. One moving. One stationary. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              8
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 4 0 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 4]]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 4]]]
              [2 [:pos [:a 2]]]
              [2 [:pos [:b 4]]]
              [2 [:coll-start #{:b :a}]]
              [3 [:pos [:a 3]]]
              [3 [:pos [:b 4]]]
              [4 [:pos [:a 4]]]
              [4 [:pos [:b 4]]]
              [5 [:pos [:a 5]]]
              [5 [:pos [:b 4]]]
              [6 [:coll-end #{:b :a}]]
              [6 [:pos [:a 6]]]
              [6 [:pos [:b 4]]]
              [7 [:pos [:a 7]]]
              [7 [:pos [:b 4]]]])))
  (testing "2 particles. Both moving. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              5
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 5 -1 1]]]])
             [[0.000 [:pos [:a 0]]]
              [0.000 [:pos [:b 5]]]
              [1.000 [:pos [:a 1]]]
              [1.000 [:pos [:b 4]]]
              [1.500 [:coll-start #{:b :a}]]
              [2.000 [:pos [:a 2]]]
              [2.000 [:pos [:b 3]]]
              [3.000 [:pos [:a 3]]]
              [3.000 [:pos [:b 2]]]
              [3.500 [:coll-end #{:b :a}]]
              [4.000 [:pos [:a 4]]]
              [4.000 [:pos [:b 1]]]])))
  (testing "3 particles. One moving. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              8
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 3 0 1]]]
               [0 [:add [:c 5 0 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 3]]]
              [0 [:pos [:c 5]]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 3]]]
              [1 [:pos [:c 5]]]
              [1 [:coll-start #{:b :a}]]
              [2 [:pos [:a 2]]]
              [2 [:pos [:b 3]]]
              [2 [:pos [:c 5]]]
              [3 [:pos [:a 3]]]
              [3 [:pos [:b 3]]]
              [3 [:pos [:c 5]]]
              [3 [:coll-start #{:c :a}]]
              [4 [:pos [:a 4]]]
              [4 [:pos [:b 3]]]
              [4 [:pos [:c 5]]]
              [5 [:coll-end #{:b :a}]]
              [5 [:pos [:a 5]]]
              [5 [:pos [:b 3]]]
              [5 [:pos [:c 5]]]
              [6 [:pos [:a 6]]]
              [6 [:pos [:b 3]]]
              [6 [:pos [:c 5]]]
              [7 [:coll-end #{:c :a}]]
              [7 [:pos [:a 7]]]
              [7 [:pos [:b 3]]]
              [7 [:pos [:c 5]]]])))
  (testing "3 particles. One moving. The others overlapping. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              6
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 3 0 1]]]
               [0 [:add [:c 3 0 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 3]]]
              [0 [:pos [:c 3]]]
              [0 [:coll-start #{:c :b}]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 3]]]
              [1 [:pos [:c 3]]]
              [1 [:coll-start #{:b :a}]]
              [1 [:coll-start #{:c :a}]]
              [2 [:pos [:a 2]]]
              [2 [:pos [:b 3]]]
              [2 [:pos [:c 3]]]
              [3 [:pos [:a 3]]]
              [3 [:pos [:b 3]]]
              [3 [:pos [:c 3]]]
              [4 [:pos [:a 4]]]
              [4 [:pos [:b 3]]]
              [4 [:pos [:c 3]]]
              [5 [:coll-end #{:b :a}]]
              [5 [:pos [:a 5]]]
              [5 [:pos [:b 3]]]
              [5 [:pos [:c 3]]]
              [5 [:coll-end #{:c :a}]]]))))

(deftest collision-network-tests-2
  (testing "2 particles. One moving. One stationary. Response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-2))
              0
              5
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 4 0 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 4]]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 4]]]
              [2 [:pos [:a 2]]]
              [2 [:pos [:b 4]]]
              [2 [:coll-start #{:b :a}]]
              [2 [:coll-end #{:b :a}]]
              [3 [:pos [:a 1]]]
              [3 [:pos [:b 4]]]
              [4 [:pos [:a 0]]]
              [4 [:pos [:b 4]]]])))
  (testing "2 particles. Both moving. Response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-2))
              0
              5
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 5 -1 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 5]]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 4]]]
              [1.5 [:coll-start #{:b :a}]]
              [1.5 [:coll-end #{:b :a}]]
              [2 [:pos [:a 1]]]
              [2 [:pos [:b 4]]]
              [3 [:pos [:a 0]]]
              [3 [:pos [:b 5]]]
              [4 [:pos [:a -1]]]
              [4 [:pos [:b 6]]]])))
  (testing "3 particles. One moving. Two stationary and overlapping. Response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-2))
              0
              5
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 4 0 1]]]
               [0 [:add [:c 4 0 1]]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 4]]]
              [0 [:pos [:c 4]]]
              [0 [:coll-start #{:c :b}]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 4]]]
              [1 [:pos [:c 4]]]
              [2 [:pos [:a 2]]]
              [2 [:pos [:b 4]]]
              [2 [:pos [:c 4]]]
              [2 [:coll-start #{:b :a}]]
              [2 [:coll-start #{:c :a}]]
              [2 [:coll-end #{:b :a}]]
              [2 [:coll-end #{:c :a}]]
              [3 [:pos [:a 1]]]
              [3 [:pos [:b 4]]]
              [3 [:pos [:c 4]]]
              [4 [:pos [:a 0]]]
              [4 [:pos [:b 4]]]
              [4 [:pos [:c 4]]]]))))

(deftest collision-network-tests-3
  (testing "2 particles. Both Stationary. Initially intersecting. One object removed."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              4
              [[0 [:add [:a 0 0 1]]]
               [0 [:add [:b 0 0 1]]]
               [2 [:rem :a]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 0]]]
              [0 [:coll-start #{:b :a}]]
              [1 [:pos [:a 0]]]
              [1 [:pos [:b 0]]]
              [2 [:coll-end #{:b :a}]]
              [2 [:pos [:a 0]]]
              [2 [:pos [:b 0]]]
              [3 [:pos [:b 0]]]])))
  (testing "2 particles. One moving. One object removed at expected :coll-start."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              4
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 4 0 1]]]
               [2 [:rem :a]]])
             [[0 [:pos [:a 0]]]
              [0 [:pos [:b 4]]]
              [1 [:pos [:a 1]]]
              [1 [:pos [:b 4]]]
              [2 [:coll-start #{:b :a}]]
              [2 [:coll-end #{:b :a}]]
              [2 [:pos [:a 2]]]
              [2 [:pos [:b 4]]]
              [3 [:pos [:b 4]]]]))))
