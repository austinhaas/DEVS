(ns pettomato.rt-devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.simulators.rt-simulator-adapter :refer [rt-simulator-adapter]]
   [pettomato.devs.examples.models :refer [generator single-delay]]
   [pettomato.devs.examples.rt-models :refer [rt-generator rt-lazy-seq-generator rt-single-delay]]
   [pettomato.devs.lib.date :refer [timestamp]]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :refer [log-fn]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.rt-afap-root-coordinator :refer [rt-afap-root-coordinator]]
   [pettomato.devs.root-coordinators.rt-step-root-coordinator
    :refer [rt-step-root-coordinator step-through-to-wall-time get-sim-time
            get-clock-scale-factor set-clock-scale-factor]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.rt-atomic-simulator :refer [rt-atomic-simulator]]
   [pettomato.devs.simulators.rt-network-simulator :refer [rt-network-simulator]]))

(deftest rt-tests

  (testing "rt-atomic-simulator"
    (let [step-size 77]
      (is (event-log= [[1000 {:out ['tick]}]
                       [2000 {:out ['tick]}]
                       [3000 {:out ['tick]}]
                       [4000 {:out ['tick]}]
                       [5000 {:out ['tick]}]
                       [6000 {:out ['tick]}]
                       [7000 {:out ['tick]}]
                       [8000 {:out ['tick]}]
                       [9000 {:out ['tick]}]
                       [10000 {:out ['tick]}]]
                      (-> (rt-generator 1000 'tick step-size)
                          rt-atomic-simulator
                          (rt-afap-root-coordinator :end 10000 :step-size step-size))
                      (-> (rt-lazy-seq-generator (for [i (range)]
                                                   [1000 {:out ['tick]}])
                                                 step-size)
                          rt-atomic-simulator
                          (rt-afap-root-coordinator :end 10000 :step-size step-size))))))

  (testing "rt-network-simulator"
    (let [step-size 77
          net       (network-model {:gen (rt-generator 1000 'tick step-size)
                                    :del (rt-single-delay 200 :internal-first step-size)}
                                   [[:gen :out :del :in]
                                    [:del :out :network :out]])]
      (is (event-log= [[1200 {:out ['tick]}]
                       [2200 {:out ['tick]}]
                       [3200 {:out ['tick]}]
                       [4200 {:out ['tick]}]
                       [5200 {:out ['tick]}]
                       [6200 {:out ['tick]}]
                       [7200 {:out ['tick]}]
                       [8200 {:out ['tick]}]
                       [9200 {:out ['tick]}]]
                      (-> net
                          rt-network-simulator
                          (rt-afap-root-coordinator :end 10000 :step-size step-size)))))))

(deftest rt-confluence-tests

  (testing "internal-first"
    (is (event-log= [[200 {:out ['tick]}]
                     [300 {:out ['tick]}]
                     [400 {:out ['tick]}]]
                    (let [step-size 77
                          gen       (rt-lazy-seq-generator [[100 {:out ['tick]}]
                                                            [100 {:out ['tick]}]
                                                            [100 {:out ['tick]}]]
                                                           step-size)
                          del       (rt-single-delay 100 :internal-first step-size)
                          net       (network-model {:gen gen
                                                    :del del}
                                                   [[:gen :out :del :in]
                                                    [:del :out :network :out]])]
                      (-> net
                          rt-network-simulator
                          (rt-afap-root-coordinator :end 500))))))

  (testing "external-first"
    (is (event-log= [[400 {:out ['tick]}]]
                    (let [step-size 100
                          gen       (rt-lazy-seq-generator [[100 {:out ['tick]}]
                                                            [100 {:out ['tick]}]
                                                            [100 {:out ['tick]}]]
                                                           step-size)
                          del       (rt-single-delay 100 :external-first step-size)
                          net       (network-model {:gen gen
                                                    :del del}
                                                   [[:gen :out :del :in]
                                                    [:del :out :network :out]])]
                      (-> net
                          rt-network-simulator
                          (rt-afap-root-coordinator :end 500)))))))

(deftest mixed-rt-tests

  (testing "generator, rt-single-delay"
    (let [step-size 77
          net       (network-model {:gen (generator 1000 'tick)
                                    :del (rt-single-delay 200 :internal-first step-size)}
                                   [[:gen :out :del :in]
                                    [:del :out :network :out]])]
      (is (event-log= [[1200 {:out ['tick]}]
                       [2200 {:out ['tick]}]
                       [3200 {:out ['tick]}]
                       [4200 {:out ['tick]}]
                       [5200 {:out ['tick]}]
                       [6200 {:out ['tick]}]
                       [7200 {:out ['tick]}]
                       [8200 {:out ['tick]}]
                       [9200 {:out ['tick]}]]
                      (-> net
                          (rt-network-simulator :find-simulator (fn [k m]
                                                                  (case k
                                                                    :gen (comp rt-simulator-adapter atomic-simulator)
                                                                    :del rt-atomic-simulator                                                              )))
                          (rt-afap-root-coordinator :end 10000 :step-size step-size))))))

  (testing "rt-generator, single-delay"
    (let [step-size 77
          net       (network-model {:gen (rt-generator 1000 'tick step-size)
                                    :del (single-delay 200 :internal-first)}
                                   [[:gen :out :del :in]
                                    [:del :out :network :out]])]
      (is (event-log= [[1200 {:out ['tick]}]
                       [2200 {:out ['tick]}]
                       [3200 {:out ['tick]}]
                       [4200 {:out ['tick]}]
                       [5200 {:out ['tick]}]
                       [6200 {:out ['tick]}]
                       [7200 {:out ['tick]}]
                       [8200 {:out ['tick]}]
                       [9200 {:out ['tick]}]]
                      (-> net
                          (rt-network-simulator :find-simulator (fn [k m]
                                                                  (case k
                                                                    :gen rt-atomic-simulator
                                                                    :del (comp rt-simulator-adapter atomic-simulator))))
                          (rt-afap-root-coordinator :end 10000 :step-size step-size)))))))

(deftest scale-factor-tests
  (let [output    (atom [])
        step-size 100
        rc        (-> (generator 1000 'tick)
                      atomic-simulator
                      rt-simulator-adapter
                      (rt-step-root-coordinator 0
                                                :scale     1.0
                                                :output-fn (fn [event-log]
                                                             (when (seq event-log)
                                                               (swap! output into event-log)))))
        expected  [[1000 {:out ['tick]}]
                   [2000 {:out ['tick]}]
                   [3000 {:out ['tick]}]
                   [4000 {:out ['tick]}]
                   [5000 {:out ['tick]}]
                   [6000 {:out ['tick]}]]]
    (is (= 1.0 (get-clock-scale-factor rc)))
    (-> rc
        (step-through-to-wall-time 1000)
        (set-clock-scale-factor 2000 0.0)
        (step-through-to-wall-time 3000)
        (step-through-to-wall-time 4000)
        (set-clock-scale-factor 5000 1.0)
        (step-through-to-wall-time 6000)
        (set-clock-scale-factor 7000 2.0)
        (step-through-to-wall-time 8000))
    (is (event-log= expected @output))))
