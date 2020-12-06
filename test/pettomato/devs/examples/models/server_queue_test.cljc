(ns pettomato.devs.examples.models.server-queue-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs :as devs :refer [infinity network-model trace *trace* output=]]
   [pettomato.devs.examples.models :refer [lazy-seq-generator]]
   [pettomato.devs.examples.models.server-queue :refer [reset-next-id! server]]
   [pettomato.lib.random :as rand]))

(defn report [log]
  (let [log          (->> log
                          (map second)
                          (mapcat :out)
                          (remove nil?)
                          (map #(assoc % :delay (- (:start-time %) (:arrival-time %)))))
        start-delays (map :delay log)]
    {:total-jobs    (count log)
     :total-workers (count (distinct (map :worker log)))
     :ave-delay     (/ (reduce + start-delays) (count start-delays))
     :max-delay     (apply max start-delays)}))

(deftest complex-example-test

  (is (= {:total-jobs    100
          :total-workers 10
          :ave-delay     (/ 2489 100)
          :max-delay     48}
         (binding [*trace*        false
                   *print-length* 1000]
           (let [gen (lazy-seq-generator
                      (take 100
                            (for [i (range)]
                              [(+ 1 (rand/rand-int 10)) {:out [{:id     (str "job-" i)
                                                                :effort (+ 1 (rand/rand-int 100))}]}])))
                 srv (server :server)
                 net (network-model
                      {:gen    gen
                       :server srv}
                      [[:gen :out :server :in identity]
                       [:gen :out :network :gen-out identity]
                       [:server :out :network :out identity]
                       [:server :structure :network :structure identity]])]
             (reset-next-id!)
             (rand/with-random-seed 0
               (-> (devs/run (devs/network-simulator net) :start 0 :end 1000)
                   report)))))))
