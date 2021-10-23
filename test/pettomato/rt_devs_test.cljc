(ns pettomato.rt-devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator single-delay]]
   [pettomato.devs.lib.date :refer [timestamp]]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :refer [log-fn]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.rt-afap-root-coordinator :refer [rt-afap-root-coordinator]]
   [pettomato.devs.root-coordinators.rt-step-root-coordinator
    :refer [rt-step-root-coordinator step-through-to-wall-time get-sim-time
            get-clock-scale-factor set-clock-scale-factor]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]))

(deftest scale-factor-tests
  (let [step-size 100
        rc        (-> (generator (*R 1000) 'tick)
                      atomic-simulator
                      (rt-step-root-coordinator 0 :scale 1.0))
        expected  [[(*R 1000) {:out ['tick]}]
                   [(*R 2000) {:out ['tick]}]
                   [(*R 3000) {:out ['tick]}]
                   [(*R 4000) {:out ['tick]}]
                   [(*R 5000) {:out ['tick]}]
                   [(*R 6000) {:out ['tick]}]]]
    (is (= 1.0 (get-clock-scale-factor rc)))
    (let [[rc event-log-1] (step-through-to-wall-time rc 1000)
          rc               (set-clock-scale-factor rc 2000 0.0)
          [rc event-log-2] (step-through-to-wall-time rc 3000)
          [rc event-log-3] (step-through-to-wall-time rc 4000)
          rc               (set-clock-scale-factor rc 5000 1.0)
          [rc event-log-4] (step-through-to-wall-time rc 6000)
          rc               (set-clock-scale-factor rc 7000 2.0)
          [rc event-log-5] (step-through-to-wall-time rc 8000)
          event-log        (concat event-log-1
                                   event-log-2
                                   event-log-3
                                   event-log-4
                                   event-log-5)]
      (is (event-log= expected event-log)))))
