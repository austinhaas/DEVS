(ns pettomato.devs.parallel.models.examples-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.parallel.root-coordinators.as-fast-as-possible
    :refer [afap-root-coordinator lazy-afap-root-coordinator]]
   [pettomato.devs.parallel.models.coupled :refer [coupled-model network-id]]
   [pettomato.devs.parallel.models.examples :refer [generator
                                                    lazy-seq-generator
                                                    delay-component]]
   [pettomato.devs.parallel.simulators.atomic :refer [atomic-simulator]]
   [pettomato.devs.parallel.simulators.coupled :refer [coupled-simulator]]))

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

#_
[[10 [[:in1 1]]]
 [12 [[:in1 1]]]
 [15 [[:in1 1]]]
 [20 [[:in1 1]]]
 [25 [[:in2 2]]]]
#_
(is (eq? (-> (switch 5)
             atomic-simulator
             (afap-root-simulator 0 100)
             second)
         [[15 [[:out2 1]]]
          [20 [[:out1 1]]]
          [25 [[:out2 1]]]
          [30 [[:out2 2]]]]))

#_
(deftest switch-test
  (let [models     {:gen-1  (generator 5 10)
                    :gen-2  (generator 5 10)
                    :switch (switch 5)}
        routes     [[:gen-1 :out :switch :in1]
                    [:gen-2 :out :switch :in2]]
        simulators {:gen-1  atomic-simulator
                    :gen-2  atomic-simulator
                    :switch atomic-simulator}
        coupled    (coupled-model models routes)
        coupled'   (assoc coupled :simulators simulators)]
    (-> coupled'
        coupled-simulator
        (afap-root-coordinator 0 100)))

  )
