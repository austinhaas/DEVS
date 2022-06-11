(ns pettomato.devs.examples.tms-example-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs :as devs]
   [pettomato.devs.examples :as ex]
   [pettomato.devs.examples.tms-example :as tms]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :refer [pp-mail-log]]
   [pettomato.devs.lib.random :as rand]))

(deftest tms-example-test
  (rand/with-random-seed 0
    (let [gen (ex/generator
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
          net (devs/static-network-model
               {:gen    [gen h/zero]
                :server [srv h/zero]}
               [[:gen :out :server :in1 (keep (fn [[idx job]] (when (= idx 1) job)))]
                [:gen :out :server :in2 (keep (fn [[idx job]] (when (= idx 2) job)))]
                [:server :out :network :out]])]
      (testing "Runs without errors. Test pp-event-log, too."
        (is (< 0 (count
                  (with-out-str
                    (-> (devs/network-simulator net)
                        (devs/afap-root-coordinator :start (h/*R 0) :end (h/*R 2000))
                        pp-mail-log)))))))))
