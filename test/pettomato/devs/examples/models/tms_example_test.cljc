(ns pettomato.devs.examples.models.tms-example-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs :as devs :refer [infinity network-model trace *trace* output=]]
   [pettomato.devs.examples.models :refer [lazy-seq-generator]]
   [pettomato.devs.examples.models.tms-example :as tms]
   [pettomato.lib.random :as rand]))

(deftest tms-example-test

  ;; I think this might not be interesting because of how evenly the jobs are
  ;; distributed between the workers. We need to learn to do different
  ;; distributions.

  (binding [*trace*        false
            *print-length* 1000]
    (let [gen (lazy-seq-generator
               (take 100
                     (for [i (range)]
                       (let [dt     (+ 1 (rand/rand-int 10))
                             port   (keyword (str "out-" (inc (rand/rand-int 2))))
                             id     (str "job-" i)
                             effort (+ 1 (rand/rand-int 100))
                             job    {:id id :effort effort}]
                         [dt {port [job]}]))))
          srv (tms/network-1 10 10)
          net (network-model
               {:gen    gen
                :server srv}
               [[:gen :out-1 :server :in1 identity]
                [:gen :out-2 :server :in2 identity]
                [:server :out :network :out identity]
                [:server :structure :network :structure identity]])]
      (rand/with-random-seed 0
        (-> (devs/run (devs/network-simulator net) :start 0 :end 2000)
            devs/pp-output)))))
