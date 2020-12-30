(ns pettomato.devs.examples.models.digital-circuit-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [lazy-seq-generator]]
   [pettomato.devs.examples.models.digital-circuit :as circ]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest primitive-function-box-tests

  (testing "inverter"
    (is (event-log=
         [[0 {:out [false]}]
          [6 {:out [true]}]]
         (-> (network-model {:gen (lazy-seq-generator [[1 {:out [false]}]])
                             :inv (circ/inverter 5)}
                            [[:gen :out :inv :in identity]
                             [:inv :out :network :out identity]])
             network-simulator
             afap-root-coordinator))))

  (testing "and-gate"
    (is (event-log=
         [[0 {:out [false]}]
          [8 {:out [true]}]]
         (-> (network-model {:gen (lazy-seq-generator [[1 {:out-1 [true]}]
                                                       [2 {:out-2 [true]}]])
                             :and (circ/and-gate 5)}
                            [[:gen :out-1 :and :in-1 identity]
                             [:gen :out-2 :and :in-2 identity]
                             [:and :out :network :out identity]])
             network-simulator
             afap-root-coordinator))))

  (testing "or-gate"
    (is (event-log=
         [[0 {:out [false]}]
          [6 {:out [true]}]]
         (-> (network-model {:gen (lazy-seq-generator [[1 {:out-1 [true]}]
                                                       [2 {:out-2 [true]}]])
                             :and (circ/or-gate 5)}
                            [[:gen :out-1 :and :in-1 identity]
                             [:gen :out-2 :and :in-2 identity]
                             [:and :out :network :out identity]])
             network-simulator
             afap-root-coordinator)))))

(deftest composite-function-box-tests

  (testing "SICP, p. 280"
    (is (event-log=
         [[0 {:s [false]
              :c [false]}]
          [8  {:s [true]}]
          [11 {:c [true]}]
          [16 {:s [false]}]]
         (-> (network-model {:gen (lazy-seq-generator [[0 {:out-1 [true]}]
                                                       [8 {:out-2 [true]}]])
                             :ha  (circ/half-adder 2 3 5)}
                            [[:gen :out-1 :ha :a identity]
                             [:gen :out-2 :ha :b identity]
                             [:ha :s :network :s identity]
                             [:ha :c :network :c identity]])
             network-simulator
             afap-root-coordinator))))

  (testing "full-adder"
    (is (event-log=
         [[0  {:s [false]
               :c [false]}]
          [8  {:s [true]}]
          [24 {:s [false]
               :c [true]}]]
         (-> (network-model {:gen (lazy-seq-generator [[0 {:out-1 [true]}]
                                                       [8 {:out-2 [true]}]])
                             :ha  (circ/full-adder 2 3 5)}
                            [[:gen :out-1 :ha :a identity]
                             [:gen :out-2 :ha :b identity]
                             [:ha :s :network :s identity]
                             [:ha :c :network :c identity]])
             network-simulator
             afap-root-coordinator)))))

(deftest ripple-carry-adder-tests

  (testing "Adding 10 pairs of random 16-bit numbers"
    (rand/with-random-seed 0
      (let [as (repeatedly 10 #(rand/rand-int 65536))
            bs (repeatedly 10 #(rand/rand-int 65536))]
        (doseq [[a b] (map vector as bs)]
          (is (= (+ a b) (circ/ripple-carry-add 32 a b))))))))
