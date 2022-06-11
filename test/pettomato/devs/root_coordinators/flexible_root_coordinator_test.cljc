(ns pettomato.devs.root-coordinators.flexible-root-coordinator-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing async]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.mail :refer [mail-log=]]
   [pettomato.devs.root-coordinators.flexible-root-coordinator :as frc]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]
   #?(:cljs [goog.async.Delay :as gdelay])
   #?(:cljs [cljs.core.async :as async :refer-macros [go]])))

#?(:cljs
   (defn special-delay!
     [f initial-delay]
     (let [handle (atom nil)]
       (letfn [(step [d]
                 (if (infinite? d)
                   (reset! handle nil)
                   (->> (goog.async.Delay. #(step (f)) d)
                        (reset! handle)
                        .start)))]
         (step initial-delay))
       handle)))

#?(:cljs
   (defn cancel-special-delay! [handle]
     (.stop @handle)
     nil))

#?(:clj
   (defn start-loop-fn [pkg]
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
   :cljs
   (defn start-loop-fn [pkg]
     (special-delay!
      #(-> pkg frc/step! frc/wall-time-until-next-event)
      (frc/wall-time-until-next-event pkg))))

#?(:clj
   (defn stop-loop-fn [handle]
     (doto handle .interrupt .join)
     nil)
   :cljs
   (defn stop-loop-fn [handle]
     (when @handle
       (cancel-special-delay! handle))))

(deftest race-condition-test

  ;; To create an artificial delay, we put a buffer in a network and
  ;; include a route transducer that sleeps. As a result, the
  ;; root-coordinator will still be processing the first send-mail!
  ;; call when the second one is made. We validate that the first one
  ;; completed successfully before the 2nd was processed, because the
  ;; output was what we would expect without all the threads.

  #?(:clj
     (let [out (atom [])
           pkg (-> (m/static-network-model
                    {:buf [(m/buffer (h/*R 300))
                           h/zero]}
                    [[:network :in :buf :in (map (fn [msg] (Thread/sleep 100) msg))]
                     [:buf :out :network :out]])
                   network-simulator
                   (frc/flexible-root-coordinator
                    :output-fn     (partial swap! out into)
                    :paused?       true
                    :start-loop-fn start-loop-fn
                    :stop-loop-fn  stop-loop-fn))]
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
         (is (= mail {:out [:one]}))))))

(deftest rt-test

  #?(:clj
     (let [out (atom [])
           pkg (-> (m/static-network-model
                    {:buf [(m/buffer (h/*R 100))
                           h/zero]}
                    [[:network :in :buf :in]
                     [:buf :out :network :out]])
                   network-simulator
                   (frc/flexible-root-coordinator
                    :output-fn     (partial swap! out into)
                    :start-loop-fn start-loop-fn
                    :stop-loop-fn  stop-loop-fn))]
       (Thread/sleep 100)
       (frc/send-mail! pkg {:in [:one]})
       (Thread/sleep 200)
       (is (= 1 (count @out)))
       (let [[t mail] (first @out)]
         (is (h/<= (h/*R 200) t (h/*R 300)))
         (is (= mail {:out [:one]}))))
     :cljs
     (let [out (atom [])
           pkg (-> (m/static-network-model
                    {:buf [(m/buffer (h/*R 100))
                           h/zero]}
                    [[:network :in :buf :in]
                     [:buf :out :network :out]])
                   network-simulator
                   (frc/flexible-root-coordinator
                    :output-fn     (partial swap! out into)
                    :start-loop-fn start-loop-fn
                    :stop-loop-fn  stop-loop-fn))]
       (async done
              (go
                (async/<! (async/timeout 100))
                (frc/send-mail! pkg {:in [:one]})
                (async/<! (async/timeout 200))
                (is (= 1 (count @out)))
                (let [[t mail] (first @out)]
                  (is (h/<= (h/*R 200) t (h/*R 300)))
                  (is (= mail {:out [:one]})))
                (done))))))
