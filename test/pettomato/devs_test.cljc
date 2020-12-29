(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator lazy-seq-generator delay1]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.sim-output :refer [output=]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (binding [log/*log-level* :trace]
      (is (output= [[2 {:out ["x"]}]
                    [4 {:out ["x"]}]
                    [6 {:out ["x"]}]
                    [8 {:out ["x"]}]]
                   (-> (generator 2 "x")
                       atomic-simulator
                       (afap-root-coordinator :end 10))))))

  (testing "Specifying a non-zero start time."
    (is (output= [[7 {:out ["x"]}]
                  [9 {:out ["x"]}]]
                 (-> (generator 2 "x")
                     atomic-simulator
                     (afap-root-coordinator :start 5 :end 10)))))

  (testing "A simple network."
    (is (output= [[5  {:out ["x"]}]
                  [15 {:out ["y"]}]]
                 (let [gen (lazy-seq-generator [[0  {:out ["x"]}]
                                                [10 {:out ["y"]}]])
                       del (delay1 5)
                       net (network-model {:gen gen
                                           :del del}
                                          [[:gen :out :del :in identity]
                                           [:del :out :network :out identity]])]
                   (-> (network-simulator net)
                       afap-root-coordinator))))))

(defn switch
  "A very contrived model that is used to demonstrate confluence.

  After an initial delay of one frame, this model emits true each frame, until
  it receives a message, and then it will return false, until it receives
  another message, which switches it back to true, and so on.

  The idea is that the model does some work in its internal update, and the
  result of this work can be influenced by external messages, so the order in a
  confluent update is significant.

  If :priority is :int, then it will compute the next value before considering
  imminent messages.

  If :priority is :ext, then it will factor in the external messages before
  computing the next value."
  [& {:keys [priority]
      :or   {priority nil}}]
  (let [initial-state {:value  true
                       :output true
                       :sigma  1}
        int-update    (fn [s]     (assoc s :output (:value s) :sigma 1)) ; "Compute" the new value.
        ext-update    (fn [s e x] (-> s (update :value not) (update :sigma - e)))
        output        (fn [s]     {:out [(:output s)]})
        time-advance  :sigma
        con-update    (case priority
                        :int (fn [s x] (ext-update (int-update s) 0 x))
                        :ext (fn [s x] (int-update (ext-update s (time-advance s) x)))
                        nil)]
    (atomic-model [initial-state 0]
                  int-update
                  ext-update
                  con-update
                  output
                  time-advance)))

(deftest confluence-tests

  (testing "Confluence test #1: internal before external"
    (is (output= [[1 {:out [true]}]
                  [2 {:out [true]}]
                  [3 {:out [true]}]
                  [4 {:out [false]}]]
                 (let [gen (generator 2 true)
                       sw  (switch :priority :int)
                       net (network-model {:gen gen
                                           :sw  sw}
                                          [[:gen :out :sw :in identity]
                                           [:sw :out :network :out identity]])]
                   (-> net
                       network-simulator
                       (afap-root-coordinator :end 5))))))

  (testing "Confluence test #2: internal before external"
    (is (output= [[1 {:out [true]}]
                  [2 {:out [true]}]
                  [3 {:out [false]}]
                  [4 {:out [false]}]]
                 (let [gen (generator 2 true)
                       sw  (switch :priority :ext)
                       net (network-model {:gen gen
                                           :sw  sw}
                                          [[:gen :out :sw :in identity]
                                           [:sw :out :network :out identity]])]
                   (-> net
                       network-simulator
                       (afap-root-coordinator :end 5))))))

  (testing "Confluence test #3: default priority"
    (is (output= [[1 {:out [true]}]
                  [2 {:out [true]}]
                  [3 {:out [true]}]
                  [4 {:out [false]}]]
                 (let [gen (generator 2 true)
                       sw  (switch)
                       net (network-model {:gen gen
                                           :sw  sw}
                                          [[:gen :out :sw :in identity]
                                           [:sw :out :network :out identity]])]
                   (-> net
                       network-simulator
                       (afap-root-coordinator :end 5)))))))

(deftest deep-delay-network

  (testing "A simple, but deeply nested network"
    (let [delay-network-constructor
          (fn [del] (network-model {:del del}
                                   [[:network :in :del :in identity]
                                    [:del :out :network :out identity]]))]
      (is (output= [[7 {:out ["x"]}]
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
                     (-> (network-simulator net)
                         (afap-root-coordinator :end 10))))))))

(defn dynamic-delay-network
  "Constructs a network that contains gen and adds del at start and removes it at
  end. gen is connected to del and del is connected to the network.

  This is not generally useful; it is just for testing adding and removing
  models dynamically."
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
    (is (output= []
                 (let [net (dynamic-delay-network (generator 5 "x")
                                                  (delay1 2)
                                                  0 6)]
                   (-> (network-simulator net)
                       (afap-root-coordinator :start 0 :end 10))))))

  (testing "Remove an atomic model when it is imminent."
    (is (output= [[7 {:out ["x"]}]]
                 (let [net (dynamic-delay-network (generator 5 "x")
                                                  (delay1 2)
                                                  0 7)]
                   (-> (network-simulator net)
                       (afap-root-coordinator :start 0 :end 10))))))

  (testing "Remove an atomic model after it is imminent."
    (is (output= [[7 {:out ["x"]}]]
                 (let [net (dynamic-delay-network (generator 5 "x")
                                                  (delay1 2)
                                                  0 8)]
                   (-> (network-simulator net)
                       (afap-root-coordinator :start 0 :end 10))))))

  (testing "Adding an atomic model after it would have received input."
    (is (output= [[12 {:out ["x"]}]
                  [17 {:out ["x"]}]]
                 (let [net (dynamic-delay-network (generator 5 "x")
                                                  (delay1 2)
                                                  5 15)]
                   (-> (network-simulator net)
                       (afap-root-coordinator :start 0 :end 20))))))

  ;; Test that structure changes happen from bottom up.
  ;; Remove the parent and the child.

  )

(deftest ad-hoc-structure-change-tests

  (testing "Remove a network model"
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
                   (-> (network-simulator net)
                       afap-root-coordinator)))))

  (testing "ad-hoc network structure change test"
    ;; Note that messages get dropped when they have been delivered to a delay,
    ;; but the delay is removed in a structure change.
    (binding [log/*log-level* :trace]
     (is (output= '[[1 {:gen-out ("msg-1")}]
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
                    (-> (network-simulator net)
                        (afap-root-coordinator :start 0 :end 20))))))))
