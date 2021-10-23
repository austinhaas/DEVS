(ns pettomato.devs.examples.models.server-queue-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [lazy-seq-generator]]
   [pettomato.devs.examples.models.server-queue :refer [reset-next-id! server]]
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]
   [pettomato.devs.lib.random :as rand]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

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
         (let [gen (lazy-seq-generator
                    (take 100
                          (for [i (range)]
                            [(*R (+ 1 (rand/rand-int 10))) {:out [{:id     (str "job-" i)
                                                                   :effort (*R (+ 1 (rand/rand-int 100)))}]}])))
               srv (server :server)
               net (network-model
                    {:gen    gen
                     :server srv}
                    [[:gen :out :server :in]
                     [:gen :out :network :gen-out]
                     [:server :out :network :out]
                     [:server :petition :network :petition]])]
           (reset-next-id!)
           (rand/with-random-seed 0
             (-> (afap-root-coordinator (network-simulator net) :start (*R 0) :end (*R 1000))
                 report))))))
