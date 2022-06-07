(ns pettomato.devs.root-coordinators.threaded-rt-root-coordinator-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.mail :refer [mail-log=]]
   [pettomato.devs.root-coordinators.threaded-rt-root-coordinator :as clj-rt]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest race-condition-test

  ;; In order to cause an artificial delay, we put a buffer in a
  ;; network and include a route transducer that sleeps. As a result,
  ;; the root-coordinator will still be processing the first
  ;; send-mail! call when the second one is made. We validate that the
  ;; first one completed successfully before the 2nd was processed,
  ;; because the output was what we would expect without all the
  ;; threads.

  (let [out (atom [])
        pkg (-> (m/static-network-model
                 {:buf [(m/buffer (h/*R 300))
                        h/zero]}
                 [[:network :in :buf :in (map (fn [msg] (Thread/sleep 100) msg))]
                  [:buf :out :network :out]])
                network-simulator
                (clj-rt/rt-root-coordinator
                 :output-fn (partial swap! out into)
                 :paused? true))]
    (clj-rt/unpause! pkg)
    (future
      (clj-rt/send-mail! pkg {:in [:one]}))
    (future
      (Thread/sleep 10)
      (clj-rt/send-mail! pkg {:in [:two]}))
    (future
      (Thread/sleep 500)
      (clj-rt/pause! pkg))
    (Thread/sleep 600)
    (is (= 1 (count @out)))
    (let [[t mail] (first @out)]
      (is (h/<= (h/*R 300) t (h/*R 400)))
      (is (= mail {:out [:one]})))))
