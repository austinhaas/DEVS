(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs :as devs :refer [infinity atomic-model network-model trace *trace* output=]]
   [pettomato.devs.examples.models :refer [generator lazy-seq-generator delay1]]
   [pettomato.lib.random :as rand]))

(deftest prune-test

  (is (= {:x #{1}}
         (#'devs/prune {:x #{1}} [])))

  (is (= {:x #{1}}
         (#'devs/prune {:x #{1} :y {:z []}} [:y :z]))))

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (is (= [[2 {:out ["x"]}]
            [4 {:out ["x"]}]
            [6 {:out ["x"]}]
            [8 {:out ["x"]}]]
           (binding [*trace* false]
             (-> (generator 2 "x")
                 devs/atomic-simulator
                 (devs/run :end 10))))))

  (testing "Specifying a non-zero start time."
    (is (= [[7 {:out ["x"]}]
            [9 {:out ["x"]}]]
           (binding [*trace* false]
             (-> (generator 2 "x")
                 devs/atomic-simulator
                 (devs/run :start 5 :end 10))))))

  (testing "A simple network."
    (is (= [[5  {:out ["x"]}]
            [15 {:out ["y"]}]]
           (binding [*trace* false]
             (let [gen (lazy-seq-generator [[0  {:out ["x"]}]
                                            [10 {:out ["y"]}]])
                   del (delay1 5)
                   net (network-model {:gen gen
                                       :del del}
                                      [[:gen :out :del :in identity]
                                       [:del :out :network :out identity]])]
               (-> (devs/network-simulator net)
                   devs/run)))))))

(deftest confluence-tests-1

  (let [initial-total-state [{:total 0
                              :delta 0
                              :sigma 1}
                             0]
        int-update          (fn [s] (update s :total + (:delta s)))
        ext-update          (fn [s e x] (update s :delta + (first (:in x))))
        output              (fn [s] {:out [(:total s)]})
        time-advance        :sigma]

    (testing "Confluence test #1: internal before external"
      (is (= [[1 {:out [0]}]
              [2 {:out [0]}]
              [3 {:out [1]}]
              [4 {:out [3]}]]
             (binding [*trace* false]
               (let [gen   (generator 1 1)
                     accum (atomic-model initial-total-state
                                         int-update
                                         ext-update
                                         (fn [s x] (ext-update (int-update s) 0 x))
                                         output
                                         time-advance)
                     net   (network-model {:gen   gen
                                           :accum accum}
                                          [[:gen :out :accum :in identity]
                                           [:accum :out :network :out identity]])]
                 (-> net
                     devs/network-simulator
                     (devs/run :end 5)))))))

    (testing "Confluence test #2: external before internal"
      (is (= [[1 {:out [0]}]
              [2 {:out [1]}]
              [3 {:out [3]}]
              [4 {:out [6]}]]
             (binding [*trace* false]
               (let [gen   (generator 1 1)
                     accum (atomic-model initial-total-state
                                         int-update
                                         ext-update
                                         (fn [s x] (int-update (ext-update s (:sigma s) x)))
                                         output
                                         time-advance)
                     net   (network-model {:gen   gen
                                           :accum accum}
                                          [[:gen :out :accum :in identity]
                                           [:accum :out :network :out identity]])]
                 (-> net
                     devs/network-simulator
                     (devs/run :end 5)))))))))

(deftest confluence-tests-2

  (let [initial-total-state [{:phase :passive
                              :sigma infinity}
                             0]
        int-update          (fn [s]
                              (case (:phase s)
                                :passive s
                                :active  {:phase :alarm   :sigma 0}
                                :alarm   {:phase :passive :sigma infinity}))
        ext-update          (fn [s e x]
                              {:phase :active :sigma (first (:in x))})
        output              (fn [s]
                              (case (:phase s)
                                :active {}
                                :alarm  {:out [:bzzz]}))]

    (testing "Confluence test #1: internal before external"

      (is (= [[5 {:alarm [:bzzz]}]]
             (binding [*trace* false]
               (let [gen   (lazy-seq-generator [[0 {:out [3]}]
                                                [3 {:out [2]}]])
                     alarm (atomic-model initial-total-state
                                         int-update
                                         ext-update
                                         (fn [s x] (ext-update (int-update s) 0 x))
                                         output
                                         :sigma)
                     net   (network-model {:gen   gen
                                           :alarm alarm}
                                          [[:gen :out :alarm :in identity]
                                           [:alarm :out :network :alarm identity]])]
                 (-> net
                     devs/network-simulator
                     (devs/run :end 10)))))))

    (testing "Confluence test #2: external before internal"
      (is (= [[3 {:alarm [:bzzz]}]]
             (binding [*trace* false]
               (let [gen   (lazy-seq-generator [[0 {:out [3]}]
                                                [3 {:out [2]}]])
                     alarm (atomic-model initial-total-state
                                         int-update
                                         ext-update
                                         (fn [s x] (int-update (ext-update s (:sigma s) x)))
                                         output
                                         :sigma)
                     net   (network-model {:gen   gen
                                           :alarm alarm}
                                          [[:gen :out :alarm :in identity]
                                           [:alarm :out :network :alarm identity]])]
                 (-> net
                     devs/network-simulator
                     (devs/run :end 10)))))))

    (testing "Confluence test #1: default"
      (is (= [[5 {:alarm [:bzzz]}]]
             (binding [*trace* false]
               (let [gen   (lazy-seq-generator [[0 {:out [3]}]
                                                [3 {:out [2]}]])
                     alarm (atomic-model initial-total-state
                                         int-update
                                         ext-update
                                         nil
                                         output
                                         :sigma)
                     net   (network-model {:gen   gen
                                           :alarm alarm}
                                          [[:gen :out :alarm :in identity]
                                           [:alarm :out :network :alarm identity]])]
                 (-> net
                     devs/network-simulator
                     (devs/run :end 10)))))))))

(deftest deep-delay-network

  (testing "A deeply nested delay network"
    (let [delay-network-constructor
          (fn [del] (network-model {:del del}
                                   [[:network :in :del :in identity]
                                    [:del :out :network :out identity]]))]
      (binding [*trace* false]
        (is (= [[7 {:out ["x"]}]
                [9 {:out ["x"]}]]
               (let [gen (generator 2 "x")
                     del (-> (delay1 5)
                             delay-network-constructor
                             delay-network-constructor
                             delay-network-constructor
                             delay-network-constructor)
                     net (network-model {:gen gen
                                         :del del}
                                        [[:gen :out :del :in identity]
                                         [:del :out :network :out identity]])]
                 (-> (devs/network-simulator net)
                     (devs/run :end 10)))))))))

(defn dynamic-delay-network
  "Constructs a network that contains gen and adds del at start and removes it at
  end. gen is connected to del and del is connected to the network.

  This is not generally useful; it is just for tests."
  [gen del start end]
  (network-model {:gen  gen
                  :exec (lazy-seq-generator
                         [[start {:out [[:add-model :del del]
                                        [:connect [:gen :out :del :in identity]]
                                        [:connect [:del :out :network :out identity]]]}]
                          [end {:out [[:disconnect [:gen :out :del :in identity]]
                                      [:disconnect [:del :out :network :out identity]]
                                      [:rem-model :del]]}]])}
                 [[:exec :out :network :structure identity]]))

(deftest structure-change-tests

  (testing "Remove an atomic model before it is imminent."
    (binding [*trace* false]
      (is (output= []
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    0 6)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 10)))))))

  (testing "Remove an atomic model when it is imminent."
    (binding [*trace* false]
      (is (output= [[7 {:out ["x"]}]]
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    0 7)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 10)))))))

  (testing "Remove an atomic model after it is imminent."
    (binding [*trace* false]
      (is (output= [[7 {:out ["x"]}]]
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    0 8)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 10)))))))

  (testing "Adding an atomic model after it would have received input."
    (binding [*trace* false]
      (is (output= [[12 {:out ["x"]}]
                    [17 {:out ["x"]}]]
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    5 15)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 20)))))))

  ;; Test that structure changes happen from bottom up.
  ;; Remove the parent and the child.

  )

(deftest ad-hoc-tests

  (testing "Remove a network model"
    (binding [*trace* false]
      (is (output= [[7 {:out ["msg 1" "Good"]}]]
                   (let [gen  (lazy-seq-generator [[5 {:out ["msg 1"]}]
                                                   [10 {:out ["msg 2"]}]])
                         del  (network-model {:del  (delay1 2)
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
                     (-> (devs/network-simulator net)
                         devs/run))))))

  (testing "ad-hoc network structure change test"
    ;; Note that messages get dropped when they have been delivered to a delay,
    ;; but the delay is removed in a structure change.
    (is (= '[[1 {:gen-out ("msg-1")}]
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
             [19 {:gen-out ("msg-19"), :del-2-out ("msg-17")}]]
           (binding [*trace*        false
                     *print-length* 100]
             (let [gen   (lazy-seq-generator (for [i (range)] [1 {:out [(str "msg-" (inc i))]}]))
                   del-1 (delay1 1)
                   del-2 (delay1 2)
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
               (-> (devs/network-simulator net)
                   (devs/run :start 0 :end 20))))))))
