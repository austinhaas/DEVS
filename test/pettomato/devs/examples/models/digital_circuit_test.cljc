(ns pettomato.devs.examples.models.digital-circuit-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [lazy-seq-generator]]
   [pettomato.devs.examples.models.digital-circuit :as circ]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest primitive-function-box-tests

  (testing "inverter"
    (is (event-log=
         [[(*R 0) {:out [false]}]
          [(*R 6) {:out [true]}]]
         (-> (network-model {:gen (lazy-seq-generator [[(*R 1) {:out [false]}]])
                             :inv (circ/inverter (*R 5))}
                            [[:gen :out :inv :in]
                             [:inv :out :network :out]])
             network-simulator
             afap-root-coordinator))))

  (testing "and-gate"
    (is (event-log=
         [[(*R 0) {:out [false]}]
          [(*R 8) {:out [true]}]]
         (-> (network-model {:gen (lazy-seq-generator [[(*R 1) {:out-1 [true]}]
                                                       [(*R 2) {:out-2 [true]}]])
                             :and (circ/and-gate (*R 5))}
                            [[:gen :out-1 :and :in-1]
                             [:gen :out-2 :and :in-2]
                             [:and :out :network :out]])
             network-simulator
             afap-root-coordinator))))

  (testing "or-gate"
    (is (event-log=
         [[(*R 0) {:out [false]}]
          [(*R 6) {:out [true]}]]
         (-> (network-model {:gen (lazy-seq-generator [[(*R 1) {:out-1 [true]}]
                                                       [(*R 2) {:out-2 [true]}]])
                             :and (circ/or-gate (*R 5))}
                            [[:gen :out-1 :and :in-1]
                             [:gen :out-2 :and :in-2]
                             [:and :out :network :out]])
             network-simulator
             afap-root-coordinator)))))

(deftest composite-function-box-tests

  (testing "SICP, p. 280"
    (is (event-log=
         [[(*R 0)  {:s [false]
                    :c [false]}]
          [(*R 8)  {:s [true]}]
          [(*R 11) {:c [true]}]
          [(*R 16) {:s [false]}]]
         (-> (network-model {:gen (lazy-seq-generator [[h/epsilon {:out-1 [true]}]
                                                       [(*R 8) {:out-2 [true]}]]
                                                      h/epsilon)
                             :ha  (circ/half-adder (*R 2) (*R 3) (*R 5))}
                            [[:gen :out-1 :ha :a identity]
                             [:gen :out-2 :ha :b identity]
                             [:ha :s :network :s identity]
                             [:ha :c :network :c identity]])
             network-simulator
             afap-root-coordinator))))

  (testing "full-adder"
    (is (event-log=
         [[(*R 0)  {:s [false]
                    :c [false]}]
          [(*R 8)  {:s [true]}]
          [(*R 24) {:c [true]
                    :s [false]}]]
         (-> (network-model {:gen (lazy-seq-generator [[h/epsilon {:out-1 [true]}]
                                                       [(*R 8) {:out-2 [true]}]]
                                                      h/epsilon)
                             :ha  (circ/full-adder (*R 2) (*R 3) (*R 5))}
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
