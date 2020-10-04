(ns pettomato.devs.models.examples-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.models.atomic :refer [atomic-model?]]
   [pettomato.devs.models.coupled :refer [coupled-model coupled-model?]]
   [pettomato.devs.models.examples :refer [generator
                                           lazy-seq-generator
                                           delay-component
                                           network-1]]
   [pettomato.devs.models.executive :refer [executive-model?]]
   [pettomato.devs.models.network :refer [network-model?]]
   [pettomato.devs.models.network-structure :refer [network-name]]
   [pettomato.devs.root-coordinators.as-fast-as-possible
    :refer [afap-root-coordinator lazy-afap-root-coordinator]]
   [pettomato.devs.simulators.atomic :refer [atomic-simulator]]
   [pettomato.devs.simulators.coordinator :refer [coordinator]]
   [pettomato.devs.simulators.executive :refer [executive-simulator]]
   [pettomato.devs.simulators.network :refer [network-simulator]]
   [pettomato.devs.util :refer [*trace*]]))

(deftest generator-test
  (is (= [[10 {:out [5]}]
          [20 {:out [5]}]]
         (-> (atomic-simulator :gen (generator 5 10))
             (afap-root-coordinator 0 20)))))

(deftest lazy-seq-generator-test
  (binding [*trace* true]
   (is (= [[10 {:out [5]}]
           [20 {:out [5]}]]
          (-> (atomic-simulator :gen (lazy-seq-generator [[10 {:out [5]}]
                                                          [10 {:out [5]}]]))
              (afap-root-coordinator 0 20))))))

(defn default-sim-fns [model-name model]
  (cond
    (executive-model? model) executive-simulator
    (atomic-model?    model) atomic-simulator
    (coupled-model?   model) (partial coordinator default-sim-fns)
    (network-model?   model) (partial network-simulator default-sim-fns)))

(deftest delay-test
  (is (= [[15 {:out [5]}]
          [25 {:out [5]}]]
         (let [models      {:gen   (generator 5 10)
                            :delay (delay-component 5)}
               routes      [[:gen :out :delay :in]
                            [:delay :out network-name :out]]
               model       (coupled-model models routes)
               coordinator (coordinator default-sim-fns model)]
           (-> coordinator
               (afap-root-coordinator 0 25))))))
#_
(deftest confluence-test
  (is (eq? (-> (delay-1 10)
               atomic-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]
            [20 [[:out 2]]]]))

  ;; The second input value should be ignored, since the model is in
  ;; the busy state and external inputs have higher priority in
  ;; delay-2.
  (is (eq? (-> (delay-2 10)
               atomic-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]]))

  (is (eq? (-> (network-model
                :exec
                (executive-model
                 (-> {}
                     (register :delay (delay-1 10))
                     (connect network-id :in :delay :in)
                     (connect :delay :out network-id :out))
                 nil nil nil nil (constantly infinity)))
               network-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]
            [20 [[:out 2]]]]))

  (is (eq? (-> (network-model
                :exec
                (executive-model
                 (-> {}
                     (register :delay (delay-2 10))
                     (connect network-id :in :delay :in)
                     (connect :delay :out network-id :out))
                 nil nil nil nil (constantly infinity)))
               network-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]])))

(deftest dynamic-network-test
  #_
  (is ((fn [ev*]
         ;; This is ugly. The idea is that there should only be two
         ;; events, and they each output 5 jobs, but which jobs is not
         ;; important.
         (and (= (count ev*) 2)
              (= (first (first ev*)) 1001)
              (= (first (second ev*)) 2001)
              (= (count (second (first ev*))) 5)
              (= (count (second (second ev*))) 5)))

       ))

  (let [models      {:gen (lazy-seq-generator [[1 {:out (range 10)}]])
                     :net network-1}
        routes      [[:gen :out :net 'in1]
                     [:net 'out network-name :out]]
        model       (coupled-model models routes)
        coordinator (coordinator default-sim-fns model)]
    (binding [*trace* true]
      (-> coordinator
          (afap-root-coordinator 0 100)))))
