(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator lazy-seq-generator single-delay fixed-delay]]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (is (event-log= [[2 {:out ["x"]}]
                     [4 {:out ["x"]}]
                     [6 {:out ["x"]}]
                     [8 {:out ["x"]}]
                     [10 {:out ["x"]}]]
                    (-> (generator 2 "x")
                        atomic-simulator
                        (afap-root-coordinator :end 10)))))

  (testing "Specifying a non-zero start time."
    (is (event-log= [[7 {:out ["x"]}]
                     [9 {:out ["x"]}]]
                    (-> (generator 2 "x")
                        atomic-simulator
                        (afap-root-coordinator :start 5 :end 10)))))

  (testing "A simple network."
    (is (event-log= [[5  {:out ["x"]}]
                     [15 {:out ["y"]}]]
                    (let [gen (lazy-seq-generator [[0  {:out ["x"]}]
                                                   [10 {:out ["y"]}]])
                          del (fixed-delay 5)
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in identity]
                                              [:del :out :network :out identity]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest route-function-tests

  (testing "Using route functions"
    (is (event-log= [[5  {:out [[10]]}]
                     [15 {:out [[20]]}]]
                    (let [gen (lazy-seq-generator [[0  {:out [1]}]
                                                   [10 {:out [2]}]])
                          del (fixed-delay 5)
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in (partial * 10)]
                                              [:del :out :network :out vector]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "Default route function"
    (is (event-log= [[5  {:out [1]}]
                     [15 {:out [2]}]]
                    (let [gen (lazy-seq-generator [[0  {:out [1]}]
                                                   [10 {:out [2]}]])
                          del (fixed-delay 5)
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in]
                                              [:del :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest confluence-tests

  (testing "internal-first"
    (is (event-log= [[200 {:out ['tick]}]
                     [300 {:out ['tick]}]
                     [400 {:out ['tick]}]]
                    (let [gen (lazy-seq-generator [[100 {:out ['tick]}]
                                                   [100 {:out ['tick]}]
                                                   [100 {:out ['tick]}]])
                          del (single-delay 100 :priority :internal-first)
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in]
                                              [:del :out :network :out]])]
                      (-> net
                          network-simulator
                          (afap-root-coordinator :end 500))))))

  (testing "external-first"
    (is (event-log= [[400 {:out ['tick]}]]
                    (let [gen (lazy-seq-generator [[100 {:out ['tick]}]
                                                   [100 {:out ['tick]}]
                                                   [100 {:out ['tick]}]])
                          del (single-delay 100 :priority :external-first)
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in]
                                              [:del :out :network :out]])]
                      (-> net
                          network-simulator
                          (afap-root-coordinator :end 500)))))))

(deftest deep-delay-network

  (testing "A simple, but deeply nested network"
    (let [delay-network-constructor
          (fn [del] (network-model {:del del}
                                   [[:network :in :del :in identity]
                                    [:del :out :network :out identity]]))]
      (is (event-log= [[7 {:out ["x"]}]
                       [9 {:out ["x"]}]]
                      (let [gen (generator 2 "x")
                            del (-> (fixed-delay 5)
                                    delay-network-constructor
                                    delay-network-constructor
                                    delay-network-constructor
                                    delay-network-constructor)
                            net (network-model {:gen gen
                                                :del del}
                                               [[:gen :out :del :in identity]
                                                [:del :out :network :out identity]])]
                        (-> (network-simulator net)
                            (afap-root-coordinator :end 10))))))))

(deftest structure-change-tests

  (testing "Remove an atomic model before it is imminent."
    (is (event-log= []
                    (-> (network-model
                         {:gen  (generator 5 "x")
                          :del  (single-delay 2)
                          :exec (lazy-seq-generator
                                 [[6 {:out [[:disconnect [:gen :out :del     :in]]
                                            [:disconnect [:del :out :network :out]]
                                            [:rem-model :del]]}]])}
                         [[:gen  :out :del     :in]
                          [:del  :out :network :out]
                          [:exec :out :network :structure]])
                        network-simulator
                        (afap-root-coordinator :start 0 :end 10)))))

  (testing "Remove an atomic model when it is imminent."
    ;; Note that single-delay differs from fixed-delay here; it has an
    ;; additional transient state before sending output, so if it is removed at
    ;; the same time as it is imminent, then
    (is (event-log= [[7 {:out ["x"]}]]
                    (-> (network-model
                         {:gen  (generator 5 "x")
                          :del  (fixed-delay 2)
                          :exec (lazy-seq-generator
                                 [[7 {:out [[:disconnect [:gen :out :del     :in]]
                                            [:disconnect [:del :out :network :out]]
                                            [:rem-model :del]]}]])}
                         [[:gen  :out :del     :in]
                          [:del  :out :network :out]
                          [:exec :out :network :structure]])
                        network-simulator
                        (afap-root-coordinator :start 0 :end 10)))))

  (testing "Remove an atomic model after it is imminent."
    (is (event-log= [[7 {:out ["x"]}]]
                    (-> (network-model
                         {:gen  (generator 5 "x")
                          :del  (single-delay 2)
                          :exec (lazy-seq-generator
                                 [[8 {:out [[:disconnect [:gen :out :del     :in]]
                                            [:disconnect [:del :out :network :out]]
                                            [:rem-model :del]]}]])}
                         [[:gen  :out :del     :in]
                          [:del  :out :network :out]
                          [:exec :out :network :structure]])
                        network-simulator
                        (afap-root-coordinator :start 0 :end 10)))))

  (testing "Adding an atomic model after it would have received input, and testing that order of structure change messages doesn't matter."
    (is (event-log= [[12 {:out ["x"]}]
                     [17 {:out ["x"]}]]
                    (-> (network-model
                         {:gen  (generator 5 "x")
                          :exec (lazy-seq-generator
                                 [[5 {:out (shuffle
                                            [[:add-model :del (single-delay 2)]
                                             [:connect [:gen  :out :del     :in]]
                                             [:connect [:del  :out :network :out]]])}]
                                  [15 {:out (shuffle
                                             [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]])}]])}
                         [[:exec :out :network :structure]])
                        network-simulator
                        (afap-root-coordinator :start 0 :end 20)))))

  (testing "Remove a network model"
    (is (event-log= [[7 {:out ["msg 1" "Good"]}]]
                    (let [gen  (lazy-seq-generator [[5 {:out ["msg 1"]}]
                                                    [10 {:out ["msg 2"]}]])
                          del  (network-model {:del  (fixed-delay 2)
                                               :gen2 (lazy-seq-generator [[7 {:out ["Good"]}]
                                                                          [8 {:out ["Bad"]}]])}
                                              [[:network :in :del :in identity]
                                               [:del :out :network :out identity]
                                               [:gen2 :out :network :out identity]])
                          exec (lazy-seq-generator
                                [[7 {:out [[:disconnect [:gen :out :del :in identity]]
                                           [:disconnect [:del :out :network :out identity]]
                                           [:rem-model :del]]}]])
                          net  (network-model {:gen  gen
                                               :del  del
                                               :exec exec}
                                              [[:gen :out :del :in identity]
                                               [:del :out :network :out identity]
                                               [:exec :out :network :structure identity]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "ad-hoc network structure change test"
    ;; Note that messages get dropped when they have been delivered to a delay,
    ;; but the delay is removed in a structure change.
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
                    (let [gen   (lazy-seq-generator (for [i (range)] [1 {:out [(str "msg-" (inc i))]}]))
                          del-1 (fixed-delay 1)
                          del-2 (fixed-delay 2)
                          exec  (lazy-seq-generator
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
                                                   [:connect [:del-1 :out :network :del-1-out identity]]]}]]))
                          net   (network-model {:gen   gen
                                                :del-1 del-1
                                                :exec  exec}
                                               [[:gen :out :del-1 :in identity]
                                                [:gen :out :network :gen-out identity]
                                                [:del-1 :out :network :del-1-out identity]
                                                [:exec :out :network :structure identity]])]
                      (-> (network-simulator net)
                          (afap-root-coordinator :start 0 :end 20)))))))
