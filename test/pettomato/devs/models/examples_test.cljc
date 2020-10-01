(ns pettomato.devs.models.examples-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.root-coordinators.as-fast-as-possible
    :refer [afap-root-coordinator lazy-afap-root-coordinator]]
   [pettomato.devs.models.network-structure :refer [network-name]]
   [pettomato.devs.models.examples :refer [generator
                                           lazy-seq-generator
                                           delay-component]]
   [pettomato.devs.simulators.atomic :refer [atomic-simulator]]
   [pettomato.devs.simulators.coordinator :refer [coordinator]]))

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
         (let [models      {:gen   (generator 5 10)
                            :delay (delay-component 5)}
               routes      [[:gen :out :delay :in]
                            [:delay :out network-name :out]]
               sim-fns     (fn [model-name model]
                             (case model-name
                               :gen   atomic-simulator
                               :delay atomic-simulator))
               model       {:models models
                            :routes routes}
               coordinator (coordinator sim-fns model)]
           (-> coordinator
               (afap-root-coordinator 0 25))))))
