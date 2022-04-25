(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.models.atomic-model :refer [def-atomic-model]]
   [pettomato.devs.models.coupled-model :refer [coupled-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.coupled-simulator :refer [coupled-simulator]]))

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (is (event-log= [[(h/*R 2 0)  {:out ["x"]}]
                     [(h/*R 4 0)  {:out ["x"]}]
                     [(h/*R 6 0)  {:out ["x"]}]
                     [(h/*R 8 0)  {:out ["x"]}]
                     [(h/*R 10 0) {:out ["x"]}]]
                    (-> (m/generator (repeat [(h/*R 2 0) {:out ["x"]}]))
                        atomic-simulator
                        (afap-root-coordinator :end (h/*R 10 0))))))

  (testing "Specifying a non-zero start time."
    (is (event-log= [[(h/*R 7 0) {:out ["x"]}]
                     [(h/*R 9 0) {:out ["x"]}]]
                    (-> (m/generator (repeat [(h/*R 2 0) {:out ["x"]}]))
                        atomic-simulator
                        (afap-root-coordinator :start (h/*R 5 0) :end (h/*R 10 0))))))

  (testing "A simple network."
    (is (event-log= [[(h/*R 15 0) {:out ["x"]}]
                     [(h/*R 25 0) {:out ["y"]}]]
                    (let [gen (m/generator [[(h/*R 10) {:out ["x"]}]
                                            [(h/*R 10) {:out ["y"]}]])
                          buf (m/buffer (h/*R 5))
                          net (coupled-model {:gen [gen h/zero]
                                              :buf [buf h/zero]}
                                             [[:gen :out :buf :in]
                                              [:buf :out :network :out]])]
                      (-> (coupled-simulator net)
                          afap-root-coordinator))))))

(deftest initial-elapsed-time-test

  (is (event-log= [[(h/*R 13 0) {:out ["x"]}]
                   [(h/*R 23 0) {:out ["y"]}]]
                  (let [gen (m/generator [[(h/*R 10) {:out ["x"]}]
                                          [(h/*R 10) {:out ["y"]}]])
                        buf (m/buffer (h/*R 5))
                        net (coupled-model {:gen [gen (h/*R 2)]
                                            :buf [buf h/zero]}
                                           [[:gen :out :buf :in]
                                            [:buf :out :network :out]])]
                    (-> (coupled-simulator net)
                        afap-root-coordinator)))))

(deftest route-function-tests

  (testing "Using route functions"
    (is (event-log= [[(h/*R 15 0) {:out [["x"]]}]
                     [(h/*R 25 0) {:out [["y"]]}]]
                    (let [gen (m/generator [[(h/*R 10) {:out ["x"]}]
                                          [(h/*R 10) {:out ["y"]}]])
                          buf (m/buffer+)
                          net (coupled-model {:gen [gen h/zero]
                                              :buf [buf h/zero]}
                                             [[:gen :out :buf :in (fn [x] [(h/*R 5) x])]
                                              [:buf :out :network :out vector]])]
                      (-> (coupled-simulator net)
                          afap-root-coordinator))))))

(deftest confluence-tests

  (testing "Internal before external."
    (is (event-log= [[(h/*R 20 0) {:out ["x"]}]
                     [(h/*R 30 0) {:out ["y"]}]]
                    (let [gen (m/generator [[(h/*R 10) {:out ["x"]}]
                                            [(h/*R 10) {:out ["y"]}]])
                          buf (m/buffer (h/*R 10))
                          net (coupled-model {:gen [gen h/zero]
                                              :buf [buf h/zero]}
                                             [[:gen :out :buf :in]
                                              [:buf :out :network :out]])]
                      (-> (coupled-simulator net)
                          afap-root-coordinator)))))

  (testing "External before internal."
    (is (event-log= [[(h/*R 20 0) {:out ["x"]}]]
                    (let [gen (m/generator [[(h/*R 10) {:out ["x"]}]
                                            [(h/*R 10) {:out ["y"]}]])
                          buf (m/buffer2 (h/*R 10))
                          net (coupled-model {:gen [gen h/zero]
                                              :buf [buf h/zero]}
                                             [[:gen :out :buf :in]
                                              [:buf :out :network :out]])]
                      (-> (coupled-simulator net)
                          afap-root-coordinator))))))

(deftest deeply-nested-structure

  (testing "A trivial deeply nested structure."
    (let [coupled-constructor
          (fn [buf] (coupled-model {:buf [buf h/zero]}
                                   [[:network :in :buf :in]
                                    [:buf :out :network :out]]))]
      (is (event-log= [[(h/*R 5) {:out [0]}]
                       [(h/*R 9) {:out [2]}]]
                      (let [gen (m/generator (for [i (range)] [(h/*R 2) {:out [i]}]))
                            buf (-> (m/buffer (h/*R 3))
                                    coupled-constructor
                                    coupled-constructor
                                    coupled-constructor
                                    coupled-constructor)
                            net (coupled-model {:gen [gen h/zero]
                                                :buf [buf h/zero]}
                                               [[:gen :out :buf :in]
                                                [:buf :out :network :out]])]
                        (-> (coupled-simulator net)
                            (afap-root-coordinator :end (h/*R 10)))))))))
#_
(deftest structure-change-tests

  (testing "Remove an atomic model before it is imminent."
    (is (event-log= []
                    (-> (network-model
                         {:gen  (m/generator (h/*R 5) "x")
                          :del  (fixed-delay (h/*R 2))
                          :exec (lazy-seq-petition-generator
                                 [[(h/*R 7 -2) [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]]]])}
                         [[:gen :out :del     :in]
                          [:del :out :network :out]])
                        network-simulator
                        (afap-root-coordinator :start (h/*R 0) :end (h/*R 20))))))

  (testing "Remove an atomic model when it is imminent."
    (is (event-log= []
                    (-> (network-model
                         {:gen  (m/generator (h/*R 5) "x")
                          :del  (fixed-delay (h/*R 2))
                          :exec (lazy-seq-petition-generator
                                 [[(h/*R 7 -1) [[:disconnect [:gen :out :del     :in]]
                                              [:disconnect [:del :out :network :out]]
                                              [:rem-model :del]]]])}
                         [[:gen :out :del     :in]
                          [:del :out :network :out]])
                        network-simulator
                        (afap-root-coordinator :start (h/*R 0) :end (h/*R 20))))))

  (testing "Remove an atomic model after it is imminent."
    (is (event-log= [[(h/*R 7) {:out ["x"]}]]
                    (-> (network-model
                         {:gen  (m/generator (h/*R 5 0) "x")
                          :del  (fixed-delay (h/*R 2))
                          :exec (lazy-seq-petition-generator
                                 ;; The first message is sent instantly at 7,
                                 ;; but the structure change won't take effect
                                 ;; until the very start of 7+ε, after
                                 ;; transitions at 7 and before transitions at
                                 ;; 7+ε. :del is imminent at 7, and will
                                 ;; be removed at 7+ε.
                                 [[(h/*R 7) [[:disconnect [:gen :out :del     :in]]
                                           [:disconnect [:del :out :network :out]]
                                           [:rem-model :del]]]])}
                         [[:gen :out :del     :in]
                          [:del :out :network :out]])
                        network-simulator
                        (afap-root-coordinator :start (h/*R 0) :end (h/*R 20))))))

  (testing "Adding an atomic model when it should receive input, removing it before its last output, and testing that order of structure change messages doesn't matter."
    (is (event-log= [[(h/*R 7) {:out ["x"]}]
                     [(h/*R 12) {:out ["x"]}]]
                    (-> (network-model
                         {:gen  (m/generator (h/*R 5) "x")
                          :exec (lazy-seq-petition-generator
                                 ;; Note that these times are deltas.
                                 [[(h/*R 5 -1) (shuffle
                                              [[:add-model :del (fixed-delay (h/*R 2))]
                                               [:connect [:gen :out :del     :in]]
                                               [:connect [:del :out :network :out]]])]
                                  ;; Occurs at (h/*R 15 -1).
                                  [(h/*R 10 0) (shuffle
                                              [[:disconnect [:gen :out :del     :in]]
                                               [:disconnect [:del :out :network :out]]
                                               [:rem-model :del]])]])}
                         [])
                        network-simulator
                        (afap-root-coordinator :start (h/*R 0 0) :end (h/*R 20 0))))))

  (testing "Remove a network model"
    (is (event-log= [[(h/*R 7) {:out ["msg 1" "Good"]}]]
                    (let [gen  (m/generator [[(h/*R 5) {:out ["msg 1"]}]
                                                    [(h/*R 10) {:out ["msg 2"]}]])
                          del  (network-model {:del  (fixed-delay (h/*R 2))
                                               :gen2 (m/generator [[(h/*R 7) {:out ["Good"]}]
                                                                          [(h/*R 8) {:out ["Bad"]}]])}
                                              [[:network :in :del :in]
                                               [:del :out :network :out]
                                               [:gen2 :out :network :out]])
                          exec (lazy-seq-petition-generator
                                [[(h/*R 8) [[:disconnect [:gen :out :del :in]]
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
    (is (event-log= [[(h/*R 1) {:gen-out ["msg-1"]}]
                     [(h/*R 2) {:gen-out ["msg-2"] :del-1-out ["msg-1"]}]
                     [(h/*R 3) {:gen-out ["msg-3"] :del-1-out ["msg-2"]}]
                     [(h/*R 4) {:gen-out ["msg-4"] :del-1-out ["msg-3"]}]
                     [(h/*R 5) {:gen-out ["msg-5"]}]
                     [(h/*R 6) {:gen-out ["msg-6"]}]
                     [(h/*R 7) {:gen-out ["msg-7"] :del-2-out ["msg-5"]}]
                     [(h/*R 8) {:gen-out ["msg-8"] :del-2-out ["msg-6"]}]
                     [(h/*R 9) {:gen-out ["msg-9"] :del-2-out ["msg-7"]}]
                     [(h/*R 10) {:gen-out ["msg-10"]}]
                     [(h/*R 11) {:gen-out ["msg-11"] :del-1-out ["msg-10"]}]
                     [(h/*R 12) {:gen-out ["msg-12"] :del-1-out ["msg-11"]}]
                     [(h/*R 13) {:gen-out ["msg-13"] :del-1-out ["msg-12"]}]
                     [(h/*R 14) {:gen-out ["msg-14"] :del-1-out ["msg-13"]}]
                     [(h/*R 15) {:gen-out ["msg-15"]}]
                     [(h/*R 16) {:gen-out ["msg-16"]}]
                     [(h/*R 17) {:gen-out ["msg-17"] :del-2-out ["msg-15"]}]
                     [(h/*R 18) {:gen-out ["msg-18"] :del-2-out ["msg-16"]}]
                     [(h/*R 19) {:gen-out ["msg-19"] :del-2-out ["msg-17"]}]
                     [(h/*R 20) {:gen-out ["msg-20"]}]]
                    (let [gen   (m/generator (for [i (range)] [(h/*R 1) {:out [(str "msg-" (inc i))]}]))
                          del-1 (fixed-delay (h/*R 1))
                          del-2 (fixed-delay (h/*R 2))
                          exec  (lazy-seq-petition-generator
                                 (cycle [[(h/*R 5 -1) [[:disconnect [:gen :out :del-1 :in]]
                                                     [:disconnect [:del-1 :out :network :del-1-out]]
                                                     [:rem-model :del-1 del-1]
                                                     [:add-model :del-2 del-2]
                                                     [:connect [:gen :out :del-2 :in]]
                                                     [:connect [:del-2 :out :network :del-2-out]]]]
                                         [(h/*R 5 -1) [[:disconnect [:gen :out :del-2 :in]]
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
                          (afap-root-coordinator :start (h/*R 0) :end (h/*R 20))))))))
