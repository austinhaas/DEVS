(ns demo.collision-network-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq? pprint-ev*]]
   [pt-lib.number :refer [infinity]]
   [devs.executive-model :refer [executive-model]]
   [devs.executive-network-model :refer [executive-network-model]]
   [devs.atomic-model :refer [atomic-model]]
   [demo.integrator :refer [mult-integrator]]
   [demo.collision-detector :refer [collision-detector]]
   [demo.collision-responder :refer [collision-responder2]]
   [clojure.core.async :as async :refer [chan go <! timeout close! >! onto-chan]]
   [devs.executive-network-simulator :refer [network-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [devs.real-time-system :refer [real-time-system]]))

(defn collision-network-1 []
  (executive-network-model
   :collision-network
   (executive-model
    {:components  {:int   (mult-integrator 1)
                   :c-det (collision-detector 1)}
     :connections {:N     {[:add :a]   {:int   [:add :a]
                                        :c-det [:add :a]}
                           [:add :b]   {:int   [:add :b]
                                        :c-det [:add :b]}
                           [:add :c]   {:int   [:add :c]
                                        :c-det [:add :c]}
                           [:vel :a]   {:int   [:vel :a]
                                        :c-det [:vel :a]}
                           [:vel :b]   {:int   [:vel :b]
                                        :c-det [:vel :b]}
                           [:vel :c]   {:int   [:vel :c]
                                        :c-det [:vel :c]}}
                   :int   {[:pos :a]   {:N     [:pos :a]}
                           [:pos :b]   {:N     [:pos :b]}
                           [:pos :c]   {:N     [:pos :c]}}
                   :c-det {:coll-start {:N     :coll-start}
                           :coll-end   {:N     :coll-end}}}}
    nil nil nil nil
    (constantly infinity))))

(defn collision-network-2 []
  (executive-network-model
   :collision-network
   (executive-model
    {:components  {:int   (mult-integrator 1)
                   :c-det (collision-detector 1)
                   :c-res (collision-responder2)}
     :connections {:N     {[:add :a]   {:int   [:add :a]
                                        :c-det [:add :a]
                                        :c-res [:add :a]}
                           [:add :b]   {:int   [:add :b]
                                        :c-det [:add :b]
                                        :c-res [:add :b]}
                           [:add :c]   {:int   [:add :c]
                                        :c-det [:add :c]
                                        :c-res [:add :c]}
                           [:vel :a]   {:int   [:vel :a]
                                        :c-det [:vel :a]
                                        :c-res [:vel :a]}
                           [:vel :b]   {:int   [:vel :b]
                                        :c-det [:vel :b]
                                        :c-res [:vel :b]}
                           [:vel :c]   {:int   [:vel :c]
                                        :c-det [:vel :c]
                                        :c-res [:vel :c]}}
                   :int   {[:pos :a]   {:N     [:pos :a]}
                           [:pos :b]   {:N     [:pos :b]}
                           [:pos :c]   {:N     [:pos :c]}}
                   :c-det {:coll-start {:c-res :coll-start
                                        :N     :coll-start}
                           :coll-end   {:N     :coll-end}}
                   :c-res {[:vel :a]   {:int   [:vel :a]
                                        :c-det [:vel :a]}
                           [:vel :b]   {:int   [:vel :b]
                                        :c-det [:vel :b]}
                           [:vel :c]   {:int   [:vel :c]
                                        :c-det [:vel :c]}}}}
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
  (testing "2 particles. One moving. One stationary. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              8
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [4 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 4]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 4]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 4]]
              [2 [:coll-start #{:b :a}]]
              [3 [[:pos :a] 3]]
              [3 [[:pos :b] 4]]
              [4 [[:pos :a] 4]]
              [4 [[:pos :b] 4]]
              [5 [[:pos :a] 5]]
              [5 [[:pos :b] 4]]
              [6 [:coll-end #{:b :a}]]
              [6 [[:pos :a] 6]]
              [6 [[:pos :b] 4]]
              [7 [[:pos :a] 7]]
              [7 [[:pos :b] 4]]])))
  (testing "2 particles. Both moving. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              5
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [5 -1 1]]]])
             [[0.000 [[:pos :a] 0]]
              [0.000 [[:pos :b] 5]]
              [1.000 [[:pos :a] 1]]
              [1.000 [[:pos :b] 4]]
              [1.500 [:coll-start #{:b :a}]]
              [2.000 [[:pos :a] 2]]
              [2.000 [[:pos :b] 3]]
              [3.000 [[:pos :a] 3]]
              [3.000 [[:pos :b] 2]]
              [3.500 [:coll-end #{:b :a}]]
              [4.000 [[:pos :a] 4]]
              [4.000 [[:pos :b] 1]]]))))

(deftest collision-network-tests-2
  (testing "2 particles. One moving. One stationary. Response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-2))
              0
              5
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [4 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 4]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 4]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 4]]
              [2 [:coll-start #{:b :a}]]
              [2 [:coll-end #{:b :a}]]
              [3 [[:pos :a] 1]]
              [3 [[:pos :b] 4]]
              [4 [[:pos :a] 0]]
              [4 [[:pos :b] 4]]])))
  (testing "2 particles. Both moving. Response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-2))
              0
              5
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [5 -1 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 5]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 4]]
              [1.5 [:coll-start #{:b :a}]]
              [1.5 [:coll-end #{:b :a}]]
              [2 [[:pos :a] 1]]
              [2 [[:pos :b] 4]]
              [3 [[:pos :a] 0]]
              [3 [[:pos :b] 5]]
              [4 [[:pos :a] -1]]
              [4 [[:pos :b] 6]]]))))
