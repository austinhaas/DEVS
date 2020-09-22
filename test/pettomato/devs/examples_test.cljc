(ns pettomato.devs.examples-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.afap-root-coordinator
    :refer [afap-root-coordinator lazy-afap-root-coordinator]]
   [pettomato.devs.coupled-model :refer [coupled-model network-id]]
   [pettomato.devs.examples :refer [generator
                                    lazy-seq-generator
                                    delay-component]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.coupled-simulator :refer [coupled-simulator]]))

(deftest generator-test
  (is (= [[10 {:out [5]}]
          [20 {:out [5]}]]
         (-> (generator 5 10)
             atomic-simulator
             (afap-root-coordinator 0 20)))))

(deftest lazy-seq-generator-test
  (is (= [[10 {:out [5]}]
          [20 {:out [5]}]]
         (-> (lazy-seq-generator [[10 {:out [5]}]
                                  [10 {:out [5]}]])
             atomic-simulator
             (afap-root-coordinator 0 20)))))

(deftest delay-test
  (is (= [[15 {:out [5]}]
          [25 {:out [5]}]]
         (let [models     {:gen   (generator 5 10)
                           :delay (delay-component 5)}
               routes     [[:gen :out :delay :in]
                           [:delay :out network-id :out]]
               simulators {:gen   atomic-simulator
                           :delay atomic-simulator}
               coupled    (coupled-model models routes)
               coupled'   (assoc coupled :simulators simulators)]
           (-> coupled'
               coupled-simulator
               (afap-root-coordinator 0 25))))))
