(ns pettomato.rt-devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator delay1]]
   [pettomato.devs.lib.date :refer [timestamp]]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :refer [log-fn]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.models.atomic-model :refer [atomic-model ->rt]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.root-coordinators.rt-step-root-coordinator
    :refer [rt-step-root-coordinator step-through-to-wall-time get-sim-time
            get-clock-scale-factor set-clock-scale-factor]]
   [pettomato.devs.simulators.rt-atomic-simulator :refer [rt-atomic-simulator]]
   [pettomato.devs.simulators.rt-network-simulator :refer [rt-network-simulator]]))

(defn rt-generator [& args] (->rt (apply generator args)))
(defn rt-delay1    [& args] (->rt (apply delay1 args)))

(deftest rt-atomic-test
  (let [output       (atom [])
        rc           (-> (rt-generator 1000 'tick)
                         rt-atomic-simulator
                         (rt-step-root-coordinator 0
                                                   :scale     1.0
                                                   :output-fn (fn [event-log]
                                                                (when (seq event-log)
                                                                  (swap! output into event-log)))))
        expected     [[1000 {:out ['tick]}]
                      [2000 {:out ['tick]}]
                      [3000 {:out ['tick]}]
                      [4000 {:out ['tick]}]
                      [5000 {:out ['tick]}]
                      [6000 {:out ['tick]}]
                      [7000 {:out ['tick]}]
                      [8000 {:out ['tick]}]
                      [9000 {:out ['tick]}]
                      [10000 {:out ['tick]}]]
        step-size    100
        max-sim-time 10000]
    (loop [rc rc
           t  0]
      (let [rc' (step-through-to-wall-time rc t :max-sim-time max-sim-time)]
        (if (< (get-sim-time rc') max-sim-time)
          (recur rc' (+ t step-size))
          (is (event-log= expected @output)))))))

(deftest rt-network-test
  (let [net          (network-model {:gen (rt-generator 1000 'tick)
                                     :del (rt-delay1 200)}
                                    [[:gen :out :del :in identity]
                                     [:del :out :network :out identity]])
        output       (atom [])
        rc           (-> net
                         rt-network-simulator
                         (rt-step-root-coordinator 0
                                                   :scale     1.0
                                                   :output-fn (fn [event-log]
                                                                (when (seq event-log)
                                                                  (swap! output into event-log)))))
        expected     [[1200 {:out ['tick]}]
                      [2200 {:out ['tick]}]
                      [3200 {:out ['tick]}]
                      [4200 {:out ['tick]}]
                      [5200 {:out ['tick]}]
                      [6200 {:out ['tick]}]
                      [7200 {:out ['tick]}]
                      [8200 {:out ['tick]}]
                      [9200 {:out ['tick]}]]
        step-size    100
        max-sim-time 10000]
    (loop [rc rc
           t  0]
      (let [rc' (step-through-to-wall-time rc t :max-sim-time max-sim-time)]
        (if (< (get-sim-time rc) max-sim-time)
          (recur rc' (+ t step-size))
          (is (event-log= expected @output)))))))

(deftest scale-factor-tests
  (let [output       (atom [])
        rc           (-> (rt-generator 1000 'tick)
                         rt-atomic-simulator
                         (rt-step-root-coordinator 0
                                                   :scale     1.0
                                                   :output-fn (fn [event-log]
                                                                (when (seq event-log)
                                                                  (swap! output into event-log)))))
        expected     [[1000 {:out ['tick]}]
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
