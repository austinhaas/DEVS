(ns pettomato.devs.simulators.network-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.models.atomic :refer [atomic-model]]
   [pettomato.devs.models.executive :refer [executive-model]]
   [pettomato.devs.models.network :refer [network-model]]
   [pettomato.devs.models.network-structure :refer [network-name]]
   [pettomato.devs.models.examples :refer [generator]]
   [pettomato.devs.root-coordinators.as-fast-as-possible :refer
    [afap-root-coordinator]]
   [pettomato.devs.simulators.atomic :refer [atomic-simulator]]
   [pettomato.devs.simulators.network :refer [network-simulator]]
   [pettomato.devs.util :refer [infinity]]))

(deftest network-test
  (let [net-1 {:models {:a (generator :x 5)}
               :routes [[:a :out network-name :a-out identity]]}
        net-2 {:models {:a (generator :x 5)
                        :b (generator :y 3)}
               :routes [[:a :out network-name :a-out identity]
                        [:b :out network-name :b-out identity]]}
        exec  (executive-model
               [net-1 0]
               {net-1 net-2
                net-2 net-1}
               nil
               nil
               (constantly nil)
               (constantly 5)
               {net-1 net-2
                net-2 net-1})
        model (network-model :exec exec)]
    (is (= [[ 3 {:b-out [:y]}]
            [ 5 {:a-out [:x]}]
            [10 {:a-out [:x]}]
            [13 {:b-out [:y]}]
            [15 {:a-out [:x]}]
            [20 {:a-out [:x]}]
            [23 {:b-out [:y]}]
            [25 {:a-out [:x]}]]
           (-> (network-simulator (constantly atomic-simulator) model)
               (afap-root-coordinator 0 25))))))
