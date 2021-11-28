(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator lazy-seq-generator lazy-seq-petition-generator single-delay fixed-delay]]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.hyperreal :refer [*R]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :as logging]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

;; The output timestamps indicate the time that the messages were sent.

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (is (event-log= [[(*R 2 0)  {:out ["x"]}]
                     [(*R 4 0)  {:out ["x"]}]
                     [(*R 6 0)  {:out ["x"]}]
                     [(*R 8 0)  {:out ["x"]}]
                     [(*R 10 0) {:out ["x"]}]]
                    (-> (generator (*R 2 0) "x")
                        atomic-simulator
                        (afap-root-coordinator :end (*R 10 0))))))

  (testing "Specifying a non-zero start time."
    (is (event-log= [[(*R 7 0) {:out ["x"]}]
                     [(*R 9 0) {:out ["x"]}]]
                    (-> (generator (*R 2 0) "x")
                        atomic-simulator
                        (afap-root-coordinator :start (*R 5 0) :end (*R 10 0))))))

  (testing "A simple network."
    (is (event-log= [[(*R 15 0)  {:out ["x"]}]
                     [(*R 25 0) {:out ["y"]}]]
                    (let [gen (lazy-seq-generator [[(*R 10) {:out ["x"]}]
                                                   [(*R 10) {:out ["y"]}]])
                          del (fixed-delay (*R 5))
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in]
                                              [:del :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest route-function-tests

  (testing "Using route functions"
    (is (event-log= [[(*R 15) {:out [[10]]}]
                     [(*R 25) {:out [[20]]}]]
                    (let [gen (lazy-seq-generator [[(*R 10) {:out [1]}]
                                                   [(*R 10) {:out [2]}]])
                          del (fixed-delay (*R 5))
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in (partial * 10)]
                                              [:del :out :network :out vector]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "Default route function"
    (is (event-log= [[(*R 15) {:out [1]}]
                     [(*R 25) {:out [2]}]]
                    (let [gen (lazy-seq-generator [[(*R 10 0) {:out [1]}]
                                                   [(*R 10 0) {:out [2]}]])
                          del (fixed-delay (*R 5))
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in]
                                              [:del :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest confluence-tests

  ;; `single-delay` rejects any inputs received before it is done processing its
  ;; current input. Exactly when "done" is depends on its confluent update
  ;; function.

  (testing "internal-first"
    ;; Here the delay has priority :internal-first. This means that if it
    ;; receives a new input at exactly the same time as it is finishing the
    ;; previous input, it will finish the previous input first, before accepting
    ;; the new input.
    (is (event-log= [[(*R 20) {:out ['a]}]
                     [(*R 30) {:out ['b]}]
                     [(*R 40) {:out ['c]}]]
                    (let [gen (lazy-seq-generator [[(*R 10) {:out ['a]}]
                                                   [(*R 10) {:out ['b]}]
                                                   [(*R 10) {:out ['c]}]])
                          del (single-delay (*R 10) :priority :internal-first)
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in]
                                              [:del :out :network :out]])]
                      (-> net
                          network-simulator
                          (afap-root-coordinator :end (*R 50)))))))

  (testing "external-first"
    ;; Here the delay has priority :external-first. This means that if it
    ;; receives a new input at exactly the same time as it is finishing the
    ;; previous input, it will reject the new input because it isn't done
    ;; processing the previous input.

    ;; There's no event at (*R 30) because the input that arrives at (*R 20), with
    ;; value 'b, arrives before it is done processing the first input.
    (is (event-log= [[(*R 20) {:out ['a]}]
                     [(*R 40) {:out ['c]}]]
                    (let [gen (lazy-seq-generator [[(*R 10) {:out ['a]}]
                                                   [(*R 10) {:out ['b]}]
                                                   [(*R 10) {:out ['c]}]])
                          del (single-delay (*R 10) :priority :external-first)
                          net (network-model {:gen gen
                                              :del del}
                                             [[:gen :out :del :in]
                                              [:del :out :network :out]])]
                      (-> net
                          network-simulator
                          (afap-root-coordinator :end (*R 50 0))))))))

(deftest deep-delay-network

  (testing "A simple, but deeply nested network"
    (let [delay-network-constructor
          (fn [del] (network-model {:del del}
                                   [[:network :in :del :in identity]
                                    [:del :out :network :out identity]]))]
      (is (event-log= [[(*R 7) {:out ["x"]}]
                       [(*R 9) {:out ["x"]}]]
                      (let [gen (generator (*R 2) "x")
                            del (-> (fixed-delay (*R 5))
                                    delay-network-constructor
                                    delay-network-constructor
                                    delay-network-constructor
                                    delay-network-constructor)
                            net (network-model {:gen gen
                                                :del del}
                                               [[:gen :out :del :in identity]
                                                [:del :out :network :out identity]])]
                        (-> (network-simulator net)
                            (afap-root-coordinator :end (*R 10)))))))))

(deftest structure-change-tests

  (testing "Remove an atomic model before it is imminent."
    (is (event-log= []
                    (-> (network-model
                         {:gen  (generator (*R 5) "x")
                          :del  (fixed-delay (*R 2))
                          :exec (lazy-seq-petition-generator
                                 [[(*R 7 -2) [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]]]])}
                         [[:gen :out :del     :in]
                          [:del :out :network :out]])
                        network-simulator
                        (afap-root-coordinator :start (*R 0) :end (*R 20))))))

  (testing "Remove an atomic model when it is imminent."
    (is (event-log= []
                    (-> (network-model
                         {:gen  (generator (*R 5) "x")
                          :del  (fixed-delay (*R 2))
                          :exec (lazy-seq-petition-generator
                                 [[(*R 7 -1) [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]]]])}
                         [[:gen :out :del     :in]
                          [:del :out :network :out]])
                        network-simulator
                        (afap-root-coordinator :start (*R 0) :end (*R 20))))))

  (testing "Remove an atomic model after it is imminent."
    (is (event-log= [[(*R 7) {:out ["x"]}]]
                    (-> (network-model
                         {:gen  (generator (*R 5 0) "x")
                          :del  (fixed-delay (*R 2))
                          :exec (lazy-seq-petition-generator
                                 ;; The first message is sent instantly at 7,
                                 ;; but the structure change won't take effect
                                 ;; until the very start of 7+ε, after
                                 ;; transitions at 7 and before transitions at
                                 ;; 7+ε. :del is imminent at 7, and will
                                 ;; be removed at 7+ε.
                                 [[(*R 7) [[:disconnect [:gen :out :del     :in]]
                                           [:disconnect [:del :out :network :out]]
                                           [:rem-model :del]]]])}
                         [[:gen :out :del     :in]
                          [:del :out :network :out]])
                        network-simulator
                        (afap-root-coordinator :start (*R 0) :end (*R 20))))))

  (testing "Adding an atomic model when it should receive input, removing it before its last output, and testing that order of structure change messages doesn't matter."
    (is (event-log= [[(*R 7) {:out ["x"]}]
                     [(*R 12) {:out ["x"]}]]
                    (-> (network-model
                         {:gen  (generator (*R 5) "x")
                          :exec (lazy-seq-petition-generator
                                 ;; Note that these times are deltas.
                                 [[(*R 5 -1) (shuffle
                                              [[:add-model :del (fixed-delay (*R 2))]
                                               [:connect [:gen :out :del     :in]]
                                               [:connect [:del :out :network :out]]])]
                                  ;; Occurs at (*R 15 -1).
                                  [(*R 10 0) (shuffle
                                              [[:disconnect [:gen :out :del     :in]]
                                               [:disconnect [:del :out :network :out]]
                                               [:rem-model :del]])]])}
                         [])
                        network-simulator
                        (afap-root-coordinator :start (*R 0 0) :end (*R 20 0))))))

  (testing "Remove a network model"
    (is (event-log= [[(*R 7) {:out ["msg 1" "Good"]}]]
                    (let [gen  (lazy-seq-generator [[(*R 5) {:out ["msg 1"]}]
                                                    [(*R 10) {:out ["msg 2"]}]])
                          del  (network-model {:del  (fixed-delay (*R 2))
                                               :gen2 (lazy-seq-generator [[(*R 7) {:out ["Good"]}]
                                                                          [(*R 8) {:out ["Bad"]}]])}
                                              [[:network :in :del :in]
                                               [:del :out :network :out]
                                               [:gen2 :out :network :out]])
                          exec (lazy-seq-petition-generator
                                [[(*R 8) [[:disconnect [:gen :out :del :in]]
                                          [:disconnect [:del :out :network :out]]
                                          [:rem-model :del]]]])
                          net  (network-model {:gen  gen
                                               :del  del
                                               :exec exec}
                                              [[:gen :out :del :in]
                                               [:del :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "ad-hoc network structure change test"
    ;; Note that messages get dropped when they have been delivered to a delay,
    ;; but the delay is removed in a structure change.
    (is (event-log= [[(*R 1) {:gen-out ["msg-1"]}]
                     [(*R 2) {:gen-out ["msg-2"] :del-1-out ["msg-1"]}]
                     [(*R 3) {:gen-out ["msg-3"] :del-1-out ["msg-2"]}]
                     [(*R 4) {:gen-out ["msg-4"] :del-1-out ["msg-3"]}]
                     [(*R 5) {:gen-out ["msg-5"]}]
                     [(*R 6) {:gen-out ["msg-6"]}]
                     [(*R 7) {:gen-out ["msg-7"] :del-2-out ["msg-5"]}]
                     [(*R 8) {:gen-out ["msg-8"] :del-2-out ["msg-6"]}]
                     [(*R 9) {:gen-out ["msg-9"] :del-2-out ["msg-7"]}]
                     [(*R 10) {:gen-out ["msg-10"]}]
                     [(*R 11) {:gen-out ["msg-11"] :del-1-out ["msg-10"]}]
                     [(*R 12) {:gen-out ["msg-12"] :del-1-out ["msg-11"]}]
                     [(*R 13) {:gen-out ["msg-13"] :del-1-out ["msg-12"]}]
                     [(*R 14) {:gen-out ["msg-14"] :del-1-out ["msg-13"]}]
                     [(*R 15) {:gen-out ["msg-15"]}]
                     [(*R 16) {:gen-out ["msg-16"]}]
                     [(*R 17) {:gen-out ["msg-17"] :del-2-out ["msg-15"]}]
                     [(*R 18) {:gen-out ["msg-18"] :del-2-out ["msg-16"]}]
                     [(*R 19) {:gen-out ["msg-19"] :del-2-out ["msg-17"]}]
                     [(*R 20) {:gen-out ["msg-20"]}]]
                    (let [gen   (lazy-seq-generator (for [i (range)] [(*R 1) {:out [(str "msg-" (inc i))]}]))
                          del-1 (fixed-delay (*R 1))
                          del-2 (fixed-delay (*R 2))
                          exec  (lazy-seq-petition-generator
                                 (cycle [[(*R 5 -1) [[:disconnect [:gen :out :del-1 :in]]
                                                     [:disconnect [:del-1 :out :network :del-1-out]]
                                                     [:rem-model :del-1 del-1]
                                                     [:add-model :del-2 del-2]
                                                     [:connect [:gen :out :del-2 :in]]
                                                     [:connect [:del-2 :out :network :del-2-out]]]]
                                         [(*R 5 -1) [[:disconnect [:gen :out :del-2 :in]]
                                                     [:disconnect [:del-2 :out :network :del-2-out]]
                                                     [:rem-model :del-2 del-2]
                                                     [:add-model :del-1 del-1]
                                                     [:connect [:gen :out :del-1 :in]]
                                                     [:connect [:del-1 :out :network :del-1-out]]]]]))
                          net   (network-model {:gen   gen
                                                :del-1 del-1
                                                :exec  exec}
                                               [[:gen :out :del-1 :in]
                                                [:gen :out :network :gen-out]
                                                [:del-1 :out :network :del-1-out]])]
                      (-> (network-simulator net)
                          (afap-root-coordinator :start (*R 0) :end (*R 20))))))))
