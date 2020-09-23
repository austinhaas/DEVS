(ns pettomato.devs.example-models-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.afap-root-coordinator :refer [afap-root-coordinator lazy-afap-root-coordinator]]
   [pettomato.devs.models :refer [network-model network-name]]
   [pettomato.devs.example-models :refer [generator
                                          lazy-seq-generator
                                          delay-component]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.network-simulator :refer [network-simulator]]))

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
                           [:delay :out network-name :out]]
               simulators {:gen   atomic-simulator
                           :delay atomic-simulator}
               network    (network-model models routes)
               network'   (assoc network :simulators simulators)]
           (-> network'
               network-simulator
               (afap-root-coordinator 0 25))))))
