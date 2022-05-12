(ns pettomato.devs.examples.models.tms-example-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.examples.models.tms-example :as tms]
   [pettomato.devs.lib.event-log :refer [pp-event-log]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest tms-example-test
  (rand/with-random-seed 0
    (let [gen (m/generator
               (for [i (range 100)]
                 (let [dt     (h/*R (+ 1 (rand/rand-int 5)))
                       ;; A sin function is applied to the random
                       ;; selection of ports, to cause the server to
                       ;; continuously move workers.
                       period 50
                       x      (Math/sin (* (/ (* Math/PI 2)
                                              period)
                                           i))
                       y      (rand/rand 2.0)
                       idx    (int (/ (+ (inc x) y) 2))
                       id     (str "job-" i)
                       effort (h/*R (+ 1 (rand/rand-int 100)))
                       job    {:id id :effort effort}]
                   [dt [[idx job]]])))
          srv (tms/network 8 2)
          net (m/static-network-model
               {:gen    [gen h/zero]
                :server [srv h/zero]}
               [[:gen :out :server :in1 (keep (fn [[idx job]] (when (= idx 1) job)))]
                [:gen :out :server :in2 (keep (fn [[idx job]] (when (= idx 2) job)))]
                [:server :out :network :out]])]
      (testing "Runs without errors. Test pp-event-log, too."
        (is (< 0 (count
                  (with-out-str
                    (-> (afap-root-coordinator (network-simulator net) :start (h/*R 0) :end (h/*R 2000))
                        pp-event-log)))))))))
