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
   [pettomato.devs.lib.event-log :refer [event-log= compact-event-log]]
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
                                    :del (rt-single-delay 200 step-size :priority :internal-first)}
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
                          del       (rt-single-delay 100 step-size :priority :internal-first)
                          net       (network-model {:gen gen
                                                    :del del}
                                                   [[:gen :out :del :in]
                                                    [:del :out :network :out]])]
                      (-> net
                          rt-network-simulator
                          (rt-afap-root-coordinator :end 500 :step-size step-size))))))

  (testing "external-first"
    (is (event-log= [[400 {:out ['tick]}]]
                    (let [step-size 100
                          gen       (rt-lazy-seq-generator [[100 {:out ['tick]}]
                                                            [100 {:out ['tick]}]
                                                            [100 {:out ['tick]}]]
                                                           step-size)
                          del       (rt-single-delay 100 step-size :priority :external-first)
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
                                    :del (rt-single-delay 200 step-size :priority :internal-first)}
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
                                                                    :del rt-atomic-simulator)))
                          (rt-afap-root-coordinator :end 10000 :step-size step-size))))))

  (testing "rt-generator, single-delay"
    (let [step-size 77
          net       (network-model {:gen (rt-generator 1000 'tick step-size)
                                    :del (single-delay 200 :priority :internal-first)}
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
  (let [step-size 100
        rc        (-> (generator 1000 'tick)
                      atomic-simulator
                      rt-simulator-adapter
                      (rt-step-root-coordinator 0 :scale 1.0))
        expected  [[1000 {:out ['tick]}]
                   [2000 {:out ['tick]}]
                   [3000 {:out ['tick]}]
                   [4000 {:out ['tick]}]
                   [5000 {:out ['tick]}]
                   [6000 {:out ['tick]}]]]
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

(deftest structure-change-tests

  (testing "Remove an atomic model before it is imminent."
    (let [step-size 77]
      (is (event-log= []
                      (-> (network-model
                           {:gen  (rt-generator 5 "x" step-size)
                            :del  (rt-single-delay 2 step-size)
                            :exec (rt-lazy-seq-generator
                                   [[6 {:out [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]]}]]
                                   step-size)}
                           [[:gen  :out :del     :in]
                            [:del  :out :network :out]
                            [:exec :out :network :structure]])
                          rt-network-simulator
                          (rt-afap-root-coordinator :start 0 :end 10 :step-size step-size))))))

  ;; This test fails because network structure change messages are interleaved
  ;; with other messages during the same time point / update step.
  #_
  (testing "Remove an atomic model when it is imminent."
    (let [step-size 77]
      (is (event-log= [[7 {:out ["x"]}]]
                      (-> (network-model
                           {:gen  (rt-generator 5 "x" step-size)
                            :del  (rt-single-delay 2 step-size)
                            :exec (rt-lazy-seq-generator
                                   [[7 {:out [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]]}]]
                                   step-size)}
                           [[:gen  :out :del     :in]
                            [:del  :out :network :out]
                            [:exec :out :network :structure]])
                          rt-network-simulator
                          (rt-afap-root-coordinator :start 0 :end 10 :step-size step-size))))))

  (testing "Remove an atomic model after it is imminent."
    (let [step-size 77]
      (is (event-log= [[7 {:out ["x"]}]]
                      (-> (network-model
                           {:gen  (rt-generator 5 "x" step-size)
                            :del  (rt-single-delay 2 step-size)
                            :exec (rt-lazy-seq-generator
                                   [[8 {:out [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]]}]]
                                   step-size)}
                           [[:gen  :out :del     :in]
                            [:del  :out :network :out]
                            [:exec :out :network :structure]])
                          rt-network-simulator
                          (rt-afap-root-coordinator :start 0 :end 10 :step-size step-size))))))

  (testing "Adding an atomic model after it would have received input, and testing that order of structure change messages doesn't matter."
    (let [step-size 77]
      (is (event-log= [[12 {:out ["x"]}]
                       [17 {:out ["x"]}]]
                      (-> (network-model
                           {:gen  (rt-generator 5 "x" step-size)
                            :exec (rt-lazy-seq-generator
                                   [[5 {:out (shuffle
                                              [[:add-model :del (rt-single-delay 2 step-size)]
                                               [:connect [:gen  :out :del     :in]]
                                               [:connect [:del  :out :network :out]]])}]
                                    [15 {:out (shuffle
                                               [[:disconnect [:gen :out :del     :in]]
                                                [:disconnect [:del :out :network :out]]
                                                [:rem-model :del]])}]]
                                   step-size)}
                           [[:exec :out :network :structure]])
                          rt-network-simulator
                          (rt-afap-root-coordinator :start 0 :end 20 :step-size step-size))))))

  ;; This test fails because network structure change messages are interleaved
  ;; with other messages during the same time point / update step.
  #_
  (testing "Remove a network model"
    (is (event-log= [[7 {:out ["msg 1" "Good"]}]]
                    (let [step-size 77
                          gen       (rt-lazy-seq-generator [[5 {:out ["msg 1"]}]
                                                            [10 {:out ["msg 2"]}]]
                                                           step-size)
                          del       (network-model {:del  (rt-single-delay 2 step-size)
                                                    :gen2 (rt-lazy-seq-generator [[7 {:out ["Good"]}]
                                                                                  [8 {:out ["Bad"]}]]
                                                                                 step-size)}
                                                   [[:network :in :del :in identity]
                                                    [:del :out :network :out identity]
                                                    [:gen2 :out :network :out identity]])
                          exec      (rt-lazy-seq-generator
                                     [[7 {:out [[:disconnect [:gen :out :del :in identity]]
                                                [:disconnect [:del :out :network :out identity]]
                                                [:rem-model :del]]}]]
                                     step-size)
                          net       (network-model {:gen  gen
                                                    :del  del
                                                    :exec exec}
                                                   [[:gen :out :del :in identity]
                                                    [:del :out :network :out identity]
                                                    [:exec :out :network :structure identity]])]
                      (-> (rt-network-simulator net)
                          (rt-afap-root-coordinator :start 0 :end 1000 :step-size step-size))))))

  ;; This test fails because network structure change messages are interleaved
  ;; with other messages during the same time point / update step.
  #_
  (testing "ad-hoc network structure change test"
    ;; Note that messages get dropped when they have been delivered to a delay,
    ;; but the delay is removed in a structure change.
    (let [step-size 77]
      (is (event-log= '[[1 {:gen-out ("msg-1")}]
                        [2 {:gen-out ("msg-2"), :del-1-out ("msg-1")}]
                        [3 {:gen-out ("msg-3"), :del-1-out ("msg-2")}]
                        [4 {:gen-out ("msg-4"), :del-1-out ("msg-3")}]
                        [5 {:gen-out ("msg-5"), :del-1-out ("msg-4")}]
                        [6 {:gen-out ("msg-6")}]
                        [7 {:gen-out ("msg-7")}]
                        [8 {:gen-out ("msg-8"), :del-2-out ("msg-6")}]
                        [9 {:gen-out ("msg-9"), :del-2-out ("msg-7")}]
                        [10 {:gen-out ("msg-10"), :del-2-out ("msg-8")}]
                        [11 {:gen-out ("msg-11")}]
                        [12 {:gen-out ("msg-12"), :del-1-out ("msg-11")}]
                        [13 {:gen-out ("msg-13"), :del-1-out ("msg-12")}]
                        [14 {:gen-out ("msg-14"), :del-1-out ("msg-13")}]
                        [15 {:gen-out ("msg-15"), :del-1-out ("msg-14")}]
                        [16 {:gen-out ("msg-16")}]
                        [17 {:gen-out ("msg-17")}]
                        [18 {:gen-out ("msg-18"), :del-2-out ("msg-16")}]
                        [19 {:gen-out ("msg-19"), :del-2-out ("msg-17")}]
                        [20 {:gen-out ("msg-20"), :del-2-out ("msg-18")}]]
                      (let [gen   (rt-lazy-seq-generator (for [i (range)] [1 {:out [(str "msg-" (inc i))]}]) step-size)
                            del-1 (rt-single-delay 1 step-size)
                            del-2 (rt-single-delay 2 step-size)
                            exec  (rt-lazy-seq-generator
                                   (cycle [[5 {:out [[:disconnect [:gen :out :del-1 :in identity]]
                                                     [:disconnect [:del-1 :out :network :del-1-out identity]]
                                                     [:rem-model :del-1 del-1]
                                                     [:add-model :del-2 del-2]
                                                     [:connect [:gen :out :del-2 :in identity]]
                                                     [:connect [:del-2 :out :network :del-2-out identity]]]}]
                                           [5 {:out [[:disconnect [:gen :out :del-2 :in identity]]
                                                     [:disconnect [:del-2 :out :network :del-2-out identity]]
                                                     [:rem-model :del-2 del-2]
                                                     [:add-model :del-1 del-1]
                                                     [:connect [:gen :out :del-1 :in identity]]
                                                     [:connect [:del-1 :out :network :del-1-out identity]]]}]])
                                   step-size)
                            net   (network-model {:gen   gen
                                                  :del-1 del-1
                                                  :exec  exec}
                                                 [[:gen :out :del-1 :in identity]
                                                  [:gen :out :network :gen-out identity]
                                                  [:del-1 :out :network :del-1-out identity]
                                                  [:exec :out :network :structure identity]])]
                        (-> (rt-network-simulator net)
                            (rt-afap-root-coordinator :start 0 :end 20 :step-size step-size)
                            ;; It's not strictly necessary to compact here,
                            ;; since event-log= will do that, but it makes the
                            ;; output less confusing if the test fails.
                            compact-event-log)))))))
