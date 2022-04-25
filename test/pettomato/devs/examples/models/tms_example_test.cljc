#_
(ns pettomato.devs.examples.models.tms-example-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [lazy-seq-generator]]
   [pettomato.devs.examples.models.tms-example :as tms]
   [pettomato.devs.lib.event-log :refer [pp-event-log]]
   [pettomato.devs.lib.hyperreal :refer [*R]]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))
#_
(deftest tms-example-test
  (rand/with-random-seed 0
    (let [gen (lazy-seq-generator
               (take 100
                     (for [i (range)]
                       (let [dt       (*R (+ 1 (rand/rand-int 5)))
                             ;; A sin function is applied to the random
                             ;; selection of ports, to cause the server to
                             ;; continuously move workers.
                             period   50
                             x        (Math/sin (* (/ (* Math/PI 2)
                                                      period)
                                                   i))
                             y        (rand/rand 2.0)
                             port-idx (int (/ (+ (inc x) y) 2))
                             port     (keyword (str "out-" port-idx))
                             id       (str "job-" i)
                             effort   (*R (+ 1 (rand/rand-int 100)))
                             job      {:id id :effort effort}]
                         [dt {port [job]}]))))
          srv (tms/network-1 8 2)
          net (network-model
               {:gen    gen
                :server srv}
               [[:gen :out-1 :server :in1]
                [:gen :out-2 :server :in2]
                [:server :out :network :out]
                [:server :petition :network :petition]])]
      (testing "Runs without errors. Test pp-event-log, too."
        (is (< 0 (count
                  (with-out-str
                    (-> (afap-root-coordinator (network-simulator net) :start (*R 0) :end (*R 2000))
                        pp-event-log)))))))))
