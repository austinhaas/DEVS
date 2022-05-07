(ns pettomato.devs.examples.models.digital-circuit-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.examples.models.digital-circuit :as circ]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest primitive-function-box-tests

  (testing "inverter"
    (is (event-log=
         [[(h/*R 0 5) {:out [true]}]
          [(h/*R 5 5) {:out [false]}]]
         (-> (m/simple-network-model
              :exec
              {:gen [(m/generator [[(h/*R 1) {:pwr [true]}]
                                   [(h/*R 5) {:out [true]}]])
                     (h/*R 1)]
               :inv [(circ/inverter (h/*R 0 5))
                     h/zero]}
              [[:gen :pwr :inv :pwr]
               [:gen :out :inv :in]
               [:inv :out :network :out]])
             network-simulator
             afap-root-coordinator))))

  (testing "and-gate"
    (is (event-log=
         [[(h/*R 0 5) {:out [false]}]
          [(h/*R 3 5) {:out [true]}]]
         (-> (m/simple-network-model
              :exec
              {:gen [(m/generator [[(h/*R 1) {:pwr [true]}]
                                   [(h/*R 1) {:out-1 [true]}]
                                   [(h/*R 2) {:out-2 [true]}]])
                     (h/*R 1)]
               :and [(circ/and-gate (h/*R 0 5))
                     h/zero]}
                            [[:gen :pwr :and :pwr]
                             [:gen :out-1 :and :in-1]
                             [:gen :out-2 :and :in-2]
                             [:and :out :network :out]])
             network-simulator
             afap-root-coordinator))))

  (testing "or-gate"
    (is (event-log=
         [[(h/*R 0 5) {:out [false]}]
          [(h/*R 1 5) {:out [true]}]
          [(h/*R 7 5) {:out [false]}]]
         (-> (m/simple-network-model
              :exec
              {:gen [(m/generator [[(h/*R 1) {:pwr [true]}]
                                   [(h/*R 1) {:out-1 [true]}]
                                   [(h/*R 2) {:out-2 [true]}]
                                   [(h/*R 2) {:out-1 [false]}]
                                   [(h/*R 2) {:out-2 [false]}]])
                     (h/*R 1)]
               :or  [(circ/or-gate (h/*R 0 5))
                     h/zero]}
                            [[:gen :pwr :or :pwr]
                             [:gen :out-1 :or :in-1]
                             [:gen :out-2 :or :in-2]
                             [:or :out :network :out]])
             network-simulator
             afap-root-coordinator)))))

(deftest composite-function-box-tests

  (testing "SICP, p. 280"
    (is (event-log=
         [[(h/*R 0 3) {:c [false]}]
          [(h/*R 0 5) {:s [false]}]
          [(h/*R 1 8) {:s [true]}]
          [(h/*R 9 3) {:c [true]}]
          [(h/*R 9 8) {:s [false]}]]
         (-> (m/simple-network-model
              :exec
              {:gen [(m/generator [[(h/*R 1) {:pwr [true]}]
                                   [(h/*R 1) {:out-1 [true]}]
                                   [(h/*R 8) {:out-2 [true]}]])
                     (h/*R 1)]
               :ha  [(circ/half-adder (h/*R 0 2) (h/*R 0 3) (h/*R 0 5))
                     h/zero]}
                            [[:gen :pwr :ha :pwr]
                             [:gen :out-1 :ha :a]
                             [:gen :out-2 :ha :b]
                             [:ha :s :network :s]
                             [:ha :c :network :c]])
             network-simulator
             afap-root-coordinator))))

  (testing "full-adder"
    (is (event-log=
         [[(h/*R 0 5)  {:s [false]}]
          [(h/*R 0 8)  {:c [false]}]
          [(h/*R 1 8)  {:s [true]}]
          [(h/*R 9 16) {:c [true]
                        :s [false]}]]
         (-> (m/simple-network-model
              :exec
              {:gen [(m/generator [[(h/*R 1) {:pwr [true]}]
                                   [(h/*R 1) {:out-1 [true]}]
                                   [(h/*R 8) {:out-2 [true]}]])
                     (h/*R 1)]
               :ha  [(circ/full-adder (h/*R 0 2) (h/*R 0 3) (h/*R 0 5))
                     h/zero]}
              [[:gen :pwr :ha :pwr]
               [:gen :out-1 :ha :a]
               [:gen :out-2 :ha :b]
               [:ha :s :network :s]
               [:ha :c :network :c]])
             network-simulator
             afap-root-coordinator)))))

(deftest ripple-carry-adder-tests

  (testing "Adding 10 pairs of random 16-bit numbers"
    (rand/with-random-seed 0
      (let [as (repeatedly 10 #(rand/rand-int 65536))
            bs (repeatedly 10 #(rand/rand-int 65536))]
        (doseq [[a b] (map vector as bs)]
          (is (= (+ a b) (circ/ripple-carry-add 32 a b))))))))
