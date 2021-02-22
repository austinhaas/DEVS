(ns pettomato.rt-devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator delay1]]
   [pettomato.devs.lib.date :refer [timestamp]]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :refer [log-fn]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.models.atomic-model :refer [atomic-model ->rt]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.root-coordinators.rt-step-root-coordinator
    :refer [rt-step-root-coordinator step-through-to-wall-time get-sim-time]]
   [pettomato.devs.simulators.rt-atomic-simulator :refer [rt-atomic-simulator]]
   [pettomato.devs.simulators.rt-network-simulator :refer [rt-network-simulator]]))

;; I don't think it is possible to test the RT cljs version using clojure.test,
;; because there is no way to block the test from returning immediately.

;; It would be nice if we could block w/ async/await like this example:
;; https://stackoverflow.com/questions/951021/what-is-the-javascript-version-of-sleep/39914235#39914235

(defn rt-generator [& args] (->rt (apply generator args)))
(defn rt-delay1    [& args] (->rt (apply delay1 args)))

#?(:clj
   (deftest rt-atomic-test
     (let [output       (atom [])
           rc           (-> (rt-generator 1000 'tick)
                            rt-atomic-simulator
                            (rt-step-root-coordinator (timestamp)
                                                      :scale     10.0
                                                      :output-fn (fn [event-log]
                                                                   (when (seq event-log)
                                                                     (swap! output into event-log)))))
           expected     [[1000 {:out ['tick]}]
                         [2000 {:out ['tick]}]
                         [3000 {:out ['tick]}]
                         [4000 {:out ['tick]}]
                         [5000 {:out ['tick]}]
                         [6000 {:out ['tick]}]
                         [7000 {:out ['tick]}]
                         [8000 {:out ['tick]}]
                         [9000 {:out ['tick]}]
                         [10000 {:out ['tick]}]]
           step-size    100
           max-sim-time 10000]
       (loop [rc rc]
         (Thread/sleep step-size)
         (let [t   (timestamp)
               rc' (step-through-to-wall-time rc t :max-sim-time max-sim-time)]
           (if (< (get-sim-time rc') max-sim-time)
             (recur rc')
             (is (event-log= expected @output))))))))

#?(:cljs

   (comment

     (let [output       (atom [])
           rc           (-> (rt-generator 1000 'tick)
                            rt-atomic-simulator
                            (rt-step-root-coordinator (timestamp)
                                                      :scale     1.0
                                                      :output-fn (fn [event-log]
                                                                   (when (seq event-log)
                                                                     (swap! output into event-log)))))
           expected     [[1000 {:out ['tick]}]
                         [2000 {:out ['tick]}]
                         [3000 {:out ['tick]}]
                         [4000 {:out ['tick]}]
                         [5000 {:out ['tick]}]
                         [6000 {:out ['tick]}]
                         [7000 {:out ['tick]}]
                         [8000 {:out ['tick]}]
                         [9000 {:out ['tick]}]
                         [10000 {:out ['tick]}]]
           step-size    1000
           max-sim-time 10000]
       (letfn [(tick [rc]
                 (let [t   (timestamp)
                       rc' (step-through-to-wall-time rc t :max-sim-time max-sim-time)]
                   (println "tick: " t)
                   (if (< (get-sim-time rc) max-sim-time)
                     (js/setTimeout tick step-size rc')
                     (println "test:" (is (event-log= expected @output))))))]
         (js/setTimeout tick step-size rc)
         nil))

     ))

#?(:clj
   (deftest rt-network-test
     (let [net          (network-model {:gen (rt-generator 1000 'tick)
                                        :del (rt-delay1 200)}
                                       [[:gen :out :del :in identity]
                                        [:del :out :network :out identity]])
           output       (atom [])
           rc           (-> net
                            rt-network-simulator
                            (rt-step-root-coordinator (timestamp)
                                                      :scale     10.0
                                                      :output-fn (fn [event-log]
                                                                   (when (seq event-log)
                                                                     (swap! output into event-log)))))
           expected     [[1200 {:out ['tick]}]
                         [2200 {:out ['tick]}]
                         [3200 {:out ['tick]}]
                         [4200 {:out ['tick]}]
                         [5200 {:out ['tick]}]
                         [6200 {:out ['tick]}]
                         [7200 {:out ['tick]}]
                         [8200 {:out ['tick]}]
                         [9200 {:out ['tick]}]]
           step-size    100
           max-sim-time 10000]
       (loop [rc rc]
         (Thread/sleep step-size)
         (let [t   (timestamp)
               rc' (step-through-to-wall-time rc t :max-sim-time max-sim-time)]
           (if (< (get-sim-time rc) max-sim-time)
             (recur rc')
             (is (event-log= expected @output))))))))

#?(:cljs
   (comment

     (let [net          (network-model {:gen (rt-generator 1000 'tick)
                                        :del (rt-delay1 200)}
                                       [[:gen :out :del :in identity]
                                        [:del :out :network :out identity]])
           output       (atom [])
           rc           (-> net
                            rt-network-simulator
                            (rt-step-root-coordinator (timestamp)
                                                      :scale     1.0
                                                      :output-fn (fn [event-log]
                                                                   (when (seq event-log)
                                                                     (swap! output into event-log)))))
           expected     [[1200 {:out ['tick]}]
                         [2200 {:out ['tick]}]
                         [3200 {:out ['tick]}]
                         [4200 {:out ['tick]}]
                         [5200 {:out ['tick]}]
                         [6200 {:out ['tick]}]
                         [7200 {:out ['tick]}]
                         [8200 {:out ['tick]}]
                         [9200 {:out ['tick]}]]
           step-size    1000
           max-sim-time 10000]
       (letfn [(tick [rc]
                 (let [t   (timestamp)
                       rc' (step-through-to-wall-time rc t :max-sim-time max-sim-time)]
                   (println "tick: " t)
                   (if (< (get-sim-time rc) max-sim-time)
                     (js/setTimeout tick step-size rc')
                     (println (is (event-log= expected @output))))))]
         (js/setTimeout tick step-size rc)
         nil))
     ))
