(ns pettomato.devs.root-coordinators.flexible-root-coordinator-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.mail :refer [mail-log=]]
   [pettomato.devs.root-coordinators.flexible-root-coordinator :as frc]
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

  #?(:clj
     (let [out (atom [])
           pkg (-> (m/static-network-model
                    {:buf [(m/buffer (h/*R 300))
                           h/zero]}
                    [[:network :in :buf :in (map (fn [msg] (Thread/sleep 100) msg))]
                     [:buf :out :network :out]])
                   network-simulator
                   (frc/flexible-root-coordinator
                    :output-fn (partial swap! out into)
                    :paused? true
                    :start-loop-fn (fn [pkg]
                                     (doto (Thread.
                                            (fn []
                                              (try
                                                (while (not (Thread/interrupted))
                                                  (let [delta (frc/wall-time-until-next-event pkg)]
                                                    (if (infinite? delta)
                                                      (.interrupt (Thread/currentThread))
                                                      (do (Thread/sleep delta)
                                                          (frc/step! pkg)))))
                                                (catch java.lang.InterruptedException e
                                                  nil))))
                                       .start))
                    :stop-loop-fn (fn [thread]
                                    (doto thread .interrupt .join)
                                    nil)))]
       (frc/unpause! pkg)
       (future
         (frc/send-mail! pkg {:in [:one]}))
       (future
         (Thread/sleep 10)
         (frc/send-mail! pkg {:in [:two]}))
       (future
         (Thread/sleep 500)
         (frc/pause! pkg))
       (Thread/sleep 600)
       (is (= 1 (count @out)))
       (let [[t mail] (first @out)]
         (is (h/<= (h/*R 300) t (h/*R 400)))
         (is (= mail {:out [:one]}))))

     ))
