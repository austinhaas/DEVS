(ns pettomato.devs.examples.models.digital-circuit-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.examples.models.digital-circuit :as circ]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.mail :refer [mail-log=]]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest primitive-function-box-tests

  (testing "inverter"
    (is (mail-log=
         [[(h/*R 1 5) {:out [true]}]
          [(h/*R 5 5) {:out [false]}]]
         (-> (m/static-network-model
              {:gen-pwr [(m/generator [[(h/*R 1) [true]]])
                         h/zero]
               :gen-val [(m/generator [[(h/*R 5) [true]]])
                         h/zero]
               :inv     [(circ/inverter (h/*R 0 5))
                         h/zero]}
              [[:gen-pwr :out :inv :pwr]
               [:gen-val :out :inv :in]
               [:inv :out :network :out]])
             network-simulator
             afap-root-coordinator))))

  (testing "and-gate"
    (is (mail-log=
         [[(h/*R 1 5) {:out [false]}]
          [(h/*R 2 5) {:out [true]}]]
         (-> (m/static-network-model
              {:gen-pwr   [(m/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(m/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-2 [(m/generator [[(h/*R 2) [true]]])
                           h/zero]
               :and       [(circ/and-gate (h/*R 0 5))
                           h/zero]}
              [[:gen-pwr :out :and :pwr]
               [:gen-val-1 :out :and :in-1]
               [:gen-val-2 :out :and :in-2]
               [:and :out :network :out]])
             network-simulator
             afap-root-coordinator))))

  (testing "or-gate"
    (is (mail-log=
         [[(h/*R 1 5) {:out [true]}]
          [(h/*R 7 5) {:out [false]}]]
         (-> (m/static-network-model
              {:gen-pwr   [(m/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(m/generator [[(h/*R 1) [true]]
                                         [(h/*R 5) [false]]])
                           h/zero]
               :gen-val-2 [(m/generator [[(h/*R 4) [true]]
                                         [(h/*R 3) [false]]])
                           h/zero]
               :or        [(circ/or-gate (h/*R 0 5))
                           h/zero]}
              [[:gen-pwr :out :or :pwr]
               [:gen-val-1 :out :or :in-1]
               [:gen-val-2 :out :or :in-2]
               [:or :out :network :out]])
             network-simulator
             afap-root-coordinator)))))

(deftest composite-function-box-tests

  (testing "SICP, p. 280"
    (is (mail-log=
         [[(h/*R 1 3) {:c [false]}]
          [(h/*R 1 5) {:s [false]}]
          [(h/*R 1 8) {:s [true]}]
          [(h/*R 9 3) {:c [true]}]
          [(h/*R 9 8) {:s [false]}]]
         (-> (m/static-network-model
              {:gen-pwr   [(m/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(m/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-2 [(m/generator [[(h/*R 9) [true]]])
                           h/zero]
               :ha  [(circ/half-adder (h/*R 0 2) (h/*R 0 3) (h/*R 0 5))
                     h/zero]}
                            [[:gen-pwr :out :ha :pwr]
                             [:gen-val-1 :out :ha :a]
                             [:gen-val-2 :out :ha :b]
                             [:ha :s :network :s]
                             [:ha :c :network :c]])
             network-simulator
             afap-root-coordinator))))

  (testing "full-adder"
    (is (mail-log=
         [[(h/*R 1 5)  {:s [false]}]
          [(h/*R 1 8)  {:c [false]
                        :s [true]}]
          [(h/*R 9 16) {:c [true]
                        :s [false]}]]
         (-> (m/static-network-model
              {:gen-pwr   [(m/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(m/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-2 [(m/generator [[(h/*R 9) [true]]])
                           h/zero]
               :ha  [(circ/full-adder (h/*R 0 2) (h/*R 0 3) (h/*R 0 5))
                     h/zero]}
              [[:gen-pwr :out :ha :pwr]
               [:gen-val-1 :out :ha :a]
               [:gen-val-2 :out :ha :b]
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
