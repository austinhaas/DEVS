#_#_#_(ns pettomato.devs.examples.models.server-queue-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.examples.models.server-queue :refer [reset-next-id! server]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.coupled-model :refer [coupled-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.coupled-simulator :refer [coupled-simulator]]))

(defn report [log]
  (let [log          (->> log
                          (map second)
                          (mapcat :out)
                          (remove nil?)
                          (map #(assoc % :delay (h/- (:start-time %) (:arrival-time %)))))
        start-delays (map :delay log)]
    {:total-jobs    (count log)
     :total-workers (count (distinct (map :worker log)))
     :ave-delay     (/ (reduce + (map h/standard start-delays)) (count start-delays))
     :max-delay     (h/standard (apply h/max start-delays))}))

(deftest server-queue-test

  ;; This is a good demonstration of reproducibility.

  (is (= {:total-jobs    100
          :total-workers 10
          :ave-delay     (/ 2489 100)
          :max-delay     48}
         (let [gen (m/generator
                    (take 100
                          (for [i (range)]
                            [(h/*R (+ 1 (rand/rand-int 10)))
                             {:out [{:id     (str "job-" i)
                                     :effort (h/*R (+ 1 (rand/rand-int 100)))}]}])))
               srv (server :server)
               net (coupled-model
                    {:gen    [gen h/zero]
                     :server [srv h/zero]}
                    [[:gen :out :server :in]
                     [:gen :out :network :gen-out]
                     [:server :out :network :out]])]
           (reset-next-id!)
           (rand/with-random-seed 0
             (-> (coupled-simulator net)
                 (afap-root-coordinator :start (h/*R 0) :end (h/*R 1000))
                 report))))))
