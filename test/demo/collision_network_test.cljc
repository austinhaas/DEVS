(ns demo.collision-network-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq? pprint-ev*]]
   [pt-lib.coll :refer [dissoc-in]]
   [pt-lib.match :refer [match]]
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

;; First attempt at a dynamic network.
(defn collision-network-0 []
  (executive-network-model
   :net
   (executive-model
    {:output [] :sigma infinity
     :components  {:int   (mult-integrator 1)
                   :c-det (collision-detector 1)}
     :connections {:N     {:add        {:net :add}
                           :rem        {:net :rem}}
                   :c-det {:coll-start {:N   :coll-start}
                           :coll-end   {:N   :coll-end}}}}
    (fn int-update [s] (assoc s :output [] :sigma infinity))
    (fn ext-update [s e x]
      (reduce (fn [s ev]
                (match ev
                  [:add [k p v e]] (-> s
                                       (assoc-in [:connections :N   [:vel k] :int]   [:vel k])
                                       (assoc-in [:connections :N   [:vel k] :c-det] [:vel k])
                                       (assoc-in [:connections :net [:add k] :int]   [:add k])
                                       (assoc-in [:connections :net [:add k] :c-det] [:add k])
                                       (assoc-in [:connections :net [:rem k] :int]   [:rem k])
                                       (assoc-in [:connections :net [:rem k] :c-det] [:rem k])
                                       (assoc-in [:connections :int [:pos k] :N]     [:pos k])
                                       (update   :output conj [[:add k] [p v e]]))
                  [:rem k]         (-> s
                                       (dissoc-in [:connections :N   [:vel k] :int]   )
                                       (dissoc-in [:connections :N   [:vel k] :c-det] )
                                       (dissoc-in [:connections :net [:add k] :int]   )
                                       (dissoc-in [:connections :net [:add k] :c-det] )
                                       (dissoc-in [:connections :net [:rem k] :int]   )
                                       (dissoc-in [:connections :net [:rem k] :c-det] )
                                       (dissoc-in [:connections :int [:pos k] :N]     )
                                       (update    :output conj [[:rem k] nil]))))
              (assoc s :sigma 0)
              x))
    nil
    :output
    :sigma)))

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
                                        :c-det [:vel :c]}
                           [:rem :a]   {:int   [:rem :a]
                                        :c-det [:rem :a]}
                           [:rem :b]   {:int   [:rem :b]
                                        :c-det [:rem :b]}
                           [:rem :c]   {:int   [:rem :c]
                                        :c-det [:rem :c]}}
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

(deftest collision-network-tests-0
  (testing "2 particles. Both Stationary. Initially intersecting."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-0))
              0
              2
              [[0 [:add [:a 0 0 1]]]
               [0 [:add [:b 0 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 0]]
              [0 [:coll-start #{:b :a}]]
              [1 [[:pos :a] 0]]
              [1 [[:pos :b] 0]]])))
  (testing "2 particles. One moving. Initially intersecting."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-0))
              0
              3
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 0 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 0]]
              [0 [:coll-start #{:b :a}]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 0]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 0]]
              [2 [:coll-end #{:b :a}]]])))
  (testing "2 particles. One moving. One stationary. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-0))
              0
              8
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 4 0 1]]]])
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
              (network-simulator (collision-network-0))
              0
              5
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 5 -1 1]]]])
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
              [4.000 [[:pos :b] 1]]])))
    (testing "3 particles. One moving. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-0))
              0
              8
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 3 0 1]]]
               [0 [:add [:c 5 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 3]]
              [0 [[:pos :c] 5]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 3]]
              [1 [[:pos :c] 5]]
              [1 [:coll-start #{:b :a}]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 3]]
              [2 [[:pos :c] 5]]
              [3 [[:pos :a] 3]]
              [3 [[:pos :b] 3]]
              [3 [[:pos :c] 5]]
              [3 [:coll-start #{:c :a}]]
              [4 [[:pos :a] 4]]
              [4 [[:pos :b] 3]]
              [4 [[:pos :c] 5]]
              [5 [:coll-end #{:b :a}]]
              [5 [[:pos :a] 5]]
              [5 [[:pos :b] 3]]
              [5 [[:pos :c] 5]]
              [6 [[:pos :a] 6]]
              [6 [[:pos :b] 3]]
              [6 [[:pos :c] 5]]
              [7 [:coll-end #{:c :a}]]
              [7 [[:pos :a] 7]]
              [7 [[:pos :b] 3]]
              [7 [[:pos :c] 5]]])))
  (testing "3 particles. One moving. The others overlapping. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-0))
              0
              6
              [[0 [:add [:a 0 1 1]]]
               [0 [:add [:b 3 0 1]]]
               [0 [:add [:c 3 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 3]]
              [0 [[:pos :c] 3]]
              [0 [:coll-start #{:c :b}]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 3]]
              [1 [[:pos :c] 3]]
              [1 [:coll-start #{:b :a}]]
              [1 [:coll-start #{:c :a}]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 3]]
              [2 [[:pos :c] 3]]
              [3 [[:pos :a] 3]]
              [3 [[:pos :b] 3]]
              [3 [[:pos :c] 3]]
              [4 [[:pos :a] 4]]
              [4 [[:pos :b] 3]]
              [4 [[:pos :c] 3]]
              [5 [:coll-end #{:b :a}]]
              [5 [[:pos :a] 5]]
              [5 [[:pos :b] 3]]
              [5 [[:pos :c] 3]]
              [5 [:coll-end #{:c :a}]]]))))

(deftest collision-network-tests-1
  (testing "2 particles. Both Stationary. Initially intersecting."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              2
              [[0 [[:add :a] [0 0 1]]]
               [0 [[:add :b] [0 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 0]]
              [0 [:coll-start #{:b :a}]]
              [1 [[:pos :a] 0]]
              [1 [[:pos :b] 0]]])))
  (testing "2 particles. One moving. Initially intersecting."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              3
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [0 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 0]]
              [0 [:coll-start #{:b :a}]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 0]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 0]]
              [2 [:coll-end #{:b :a}]]])))
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
              [4.000 [[:pos :b] 1]]])))
  (testing "3 particles. One moving. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              8
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [3 0 1]]]
               [0 [[:add :c] [5 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 3]]
              [0 [[:pos :c] 5]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 3]]
              [1 [[:pos :c] 5]]
              [1 [:coll-start #{:b :a}]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 3]]
              [2 [[:pos :c] 5]]
              [3 [[:pos :a] 3]]
              [3 [[:pos :b] 3]]
              [3 [[:pos :c] 5]]
              [3 [:coll-start #{:c :a}]]
              [4 [[:pos :a] 4]]
              [4 [[:pos :b] 3]]
              [4 [[:pos :c] 5]]
              [5 [:coll-end #{:b :a}]]
              [5 [[:pos :a] 5]]
              [5 [[:pos :b] 3]]
              [5 [[:pos :c] 5]]
              [6 [[:pos :a] 6]]
              [6 [[:pos :b] 3]]
              [6 [[:pos :c] 5]]
              [7 [:coll-end #{:c :a}]]
              [7 [[:pos :a] 7]]
              [7 [[:pos :b] 3]]
              [7 [[:pos :c] 5]]])))
  (testing "3 particles. One moving. The others overlapping. No response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              6
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [3 0 1]]]
               [0 [[:add :c] [3 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 3]]
              [0 [[:pos :c] 3]]
              [0 [:coll-start #{:c :b}]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 3]]
              [1 [[:pos :c] 3]]
              [1 [:coll-start #{:b :a}]]
              [1 [:coll-start #{:c :a}]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 3]]
              [2 [[:pos :c] 3]]
              [3 [[:pos :a] 3]]
              [3 [[:pos :b] 3]]
              [3 [[:pos :c] 3]]
              [4 [[:pos :a] 4]]
              [4 [[:pos :b] 3]]
              [4 [[:pos :c] 3]]
              [5 [:coll-end #{:b :a}]]
              [5 [[:pos :a] 5]]
              [5 [[:pos :b] 3]]
              [5 [[:pos :c] 3]]
              [5 [:coll-end #{:c :a}]]]))))

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
              [4 [[:pos :b] 6]]])))
  (testing "3 particles. One moving. Two stationary and overlapping. Response."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-2))
              0
              5
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [4 0 1]]]
               [0 [[:add :c] [4 0 1]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 4]]
              [0 [[:pos :c] 4]]
              [0 [:coll-start #{:c :b}]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 4]]
              [1 [[:pos :c] 4]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 4]]
              [2 [[:pos :c] 4]]
              [2 [:coll-start #{:b :a}]]
              [2 [:coll-start #{:c :a}]]
              [2 [:coll-end #{:b :a}]]
              [2 [:coll-end #{:c :a}]]
              [3 [[:pos :a] 1]]
              [3 [[:pos :b] 4]]
              [3 [[:pos :c] 4]]
              [4 [[:pos :a] 0]]
              [4 [[:pos :b] 4]]
              [4 [[:pos :c] 4]]]))))

(deftest collision-network-tests-3
  (testing "2 particles. Both Stationary. Initially intersecting. One object removed."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              4
              [[0 [[:add :a] [0 0 1]]]
               [0 [[:add :b] [0 0 1]]]
               [2 [[:rem :a]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 0]]
              [0 [:coll-start #{:b :a}]]
              [1 [[:pos :a] 0]]
              [1 [[:pos :b] 0]]
              [2 [:coll-end #{:b :a}]]
              [2 [[:pos :a] 0]]
              [2 [[:pos :b] 0]]
              [3 [[:pos :b] 0]]])))
  (testing "2 particles. One moving. One object removed at expected :coll-start."
    (is (eq? (fast-as-possible-system
              (network-simulator (collision-network-1))
              0
              4
              [[0 [[:add :a] [0 1 1]]]
               [0 [[:add :b] [4 0 1]]]
               [2 [[:rem :a]]]])
             [[0 [[:pos :a] 0]]
              [0 [[:pos :b] 4]]
              [1 [[:pos :a] 1]]
              [1 [[:pos :b] 4]]
              [2 [:coll-start #{:b :a}]]
              [2 [:coll-end #{:b :a}]]
              [2 [[:pos :a] 2]]
              [2 [[:pos :b] 4]]
              [3 [[:pos :b] 4]]]))))
