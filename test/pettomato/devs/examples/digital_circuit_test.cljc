(ns pettomato.devs.examples.digital-circuit-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs :as devs]
   [pettomato.devs.examples :as ex]
   [pettomato.devs.examples.digital-circuit :as circ]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.mail :refer [mail-log=]]))

(deftest primitive-function-box-tests

  (testing "inverter"
    (is (mail-log=
         [[(h/*R 1 5) {:out [true]}]
          [(h/*R 5 5) {:out [false]}]]
         (-> (devs/static-network-model
              {:gen-pwr [(ex/generator [[(h/*R 1) [true]]])
                         h/zero]
               :gen-val [(ex/generator [[(h/*R 5) [true]]])
                         h/zero]
               :inv     [(circ/inverter (h/*R 0 5))
                         h/zero]}
              [[:gen-pwr :out :inv :pwr]
               [:gen-val :out :inv :in]
               [:inv :out :network :out]])
             devs/network-simulator
             devs/afap-root-coordinator))))

  (testing "and-gate"
    (is (mail-log=
         [[(h/*R 1 5) {:out [false]}]
          [(h/*R 2 5) {:out [true]}]]
         (-> (devs/static-network-model
              {:gen-pwr   [(ex/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(ex/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-2 [(ex/generator [[(h/*R 2) [true]]])
                           h/zero]
               :and       [(circ/and-gate (h/*R 0 5))
                           h/zero]}
              [[:gen-pwr :out :and :pwr]
               [:gen-val-1 :out :and :in-1]
               [:gen-val-2 :out :and :in-2]
               [:and :out :network :out]])
             devs/network-simulator
             devs/afap-root-coordinator))))

  (testing "or-gate"
    (is (mail-log=
         [[(h/*R 1 5) {:out [true]}]
          [(h/*R 7 5) {:out [false]}]]
         (-> (devs/static-network-model
              {:gen-pwr   [(ex/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(ex/generator [[(h/*R 1) [true]]
                                         [(h/*R 5) [false]]])
                           h/zero]
               :gen-val-2 [(ex/generator [[(h/*R 4) [true]]
                                         [(h/*R 3) [false]]])
                           h/zero]
               :or        [(circ/or-gate (h/*R 0 5))
                           h/zero]}
              [[:gen-pwr :out :or :pwr]
               [:gen-val-1 :out :or :in-1]
               [:gen-val-2 :out :or :in-2]
               [:or :out :network :out]])
             devs/network-simulator
             devs/afap-root-coordinator)))))

(deftest composite-function-box-tests

  (testing "SICP, p. 280"
    (is (mail-log=
         [[(h/*R 1 3) {:c [false]}]
          [(h/*R 1 5) {:s [false]}]
          [(h/*R 1 8) {:s [true]}]
          [(h/*R 9 3) {:c [true]}]
          [(h/*R 9 8) {:s [false]}]]
         (-> (devs/static-network-model
              {:gen-pwr   [(ex/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(ex/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-2 [(ex/generator [[(h/*R 9) [true]]])
                           h/zero]
               :ha        [(circ/half-adder (h/*R 0 2) (h/*R 0 3) (h/*R 0 5))
                           h/zero]}
              [[:gen-pwr :out :ha :pwr]
               [:gen-val-1 :out :ha :a]
               [:gen-val-2 :out :ha :b]
               [:ha :s :network :s]
               [:ha :c :network :c]])
             devs/network-simulator
             devs/afap-root-coordinator))))

  (testing "full-adder"
    (is (mail-log=
         [[(h/*R 1 5)  {:s [false]}]
          [(h/*R 1 8)  {:c [false]
                        :s [true]}]
          [(h/*R 9 16) {:c [true]
                        :s [false]}]]
         (-> (devs/static-network-model
              {:gen-pwr   [(ex/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-1 [(ex/generator [[(h/*R 1) [true]]])
                           h/zero]
               :gen-val-2 [(ex/generator [[(h/*R 9) [true]]])
                           h/zero]
               :fa        [(circ/full-adder (h/*R 0 2) (h/*R 0 3) (h/*R 0 5))
                           h/zero]}
              [[:gen-pwr :out :fa :pwr]
               [:gen-val-1 :out :fa :a]
               [:gen-val-2 :out :fa :b]
               [:fa :s :network :s]
               [:fa :c :network :c]])
             devs/network-simulator
             devs/afap-root-coordinator)))))

(deftest ripple-carry-adder-tests

  (testing "Adding 10 pairs of random 16-bit numbers"
    (rand/with-random-seed 0
      (let [as (repeatedly 10 #(rand/rand-int 65536))
            bs (repeatedly 10 #(rand/rand-int 65536))]
        (doseq [[a b] (map vector as bs)]
          (is (= (+ a b) (circ/ripple-carry-add 32 a b))))))))
