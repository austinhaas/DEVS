(ns pettomato.devs.rt-root-simulator
  "Real-time root simulator. Tries to synchronize the simulation time with a
  real-world clock."
  (:require
   #?(:cljs [goog.async.Delay :as gdelay])
   [pettomato.devs.root-simulator-base :as rsb]
   [pettomato.devs.rt-simulation-clock :as clock]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.date :refer [timestamp]]
   [pettomato.lib.log :as log]))

(defn- after [ms f]
  (if (< ms 0)
    (do (f) nil)
    #?(:clj (future (Thread/sleep ms)
                    (f))
       :cljs (let [d (goog.async.Delay. f ms)]
               (.start d)
               d))))

(defn- cancel-after [x]
  (log/info "cancel-after")
  (when x
    #?(:clj (future-cancel x)
       :cljs (.stop x)))
  nil)

(defn rt-root-simulator
  [sim sim-time output-fn]
  (atom
   {:root-sim  (rsb/root-simulator sim sim-time)
    :clock     (clock/clock (timestamp) sim-time 1.0)
    :output-fn output-fn
    :pending   nil}))

(defn- ms-until-next-event [pkg]
  (let [{:keys [root-sim clock]} pkg
        sim-time                 (clock/get-time clock (timestamp))
        tn                       (rsb/time-of-next-update root-sim)]
    #_(log/infof "sim-time: %s" sim-time)
    #_(log/infof "tn: %s" tn)
    (- tn sim-time)))

(defn- handle-next-event [pkg id]
  #_(log/infof "handle-next-event: %s" id)
  (let [root-sim  (:root-sim pkg)
        root-sim' (rsb/step root-sim)
        out-msgs  (rsb/output root-sim')
        root-sim' (rsb/clear-output root-sim')]
    (when (seq out-msgs)
      ((:output-fn pkg) out-msgs))
    (-> pkg
        (assoc :root-sim root-sim')
        (assoc :pending nil))))

(defn handle-next-event! [apkg]
  #_(log/info "handle-next-event!")
  (swap! apkg (fn [pkg]
                (let [delta (ms-until-next-event pkg)
                      id    (gensym)]
                  #_(log/infof "delta: %s, id: %s" delta id)
                  (cond
                    (<= delta 0)
                    (recur (handle-next-event pkg id))

                    (< delta infinity)
                    (assoc pkg :pending (after delta #(do (swap! apkg (fn [pkg] (handle-next-event pkg id)))
                                                          (handle-next-event! apkg))))

                    :else
                    (do #_(log/info "Nothing to do.")
                        pkg)))))
  nil)

(defn start! [apkg]
  (log/info "start!")
  (swap! apkg (fn [pkg]
                (if (= (clock/paused? (:clock pkg)))
                  (update pkg :clock clock/unpause (timestamp))
                  pkg)))
  ;; TODO: Don't do this if the clock is paused!
  (handle-next-event! apkg)
  nil)

(defn stop! [apkg]
  (log/info "stop!")
  (swap! apkg (fn [pkg]
                (if (= (clock/paused? (:clock pkg)))
                  (-> pkg
                      (update :pending cancel-after)
                      (update :clock clock/pause (timestamp)))
                  pkg)))
  nil)

(defn schedule! [apkg t msg]
  (log/infof "schedule!: %s" t)
  (swap! apkg (fn [pkg]
                (let [sim-time t
                      pkg      (-> pkg
                                   (update :pending cancel-after)
                                   (update :root-sim rsb/schedule sim-time msg))]
                  (when (not (clock/paused? (:clock pkg)))
                    (handle-next-event! apkg))
                  pkg)))
  nil)

(defn schedule-now! [apkg msg]
  (log/info "schedule-now!")
  (swap! apkg (fn [pkg]
               (-> pkg
                   (update :pending cancel-after)
                   (update :root-sim rsb/schedule (clock/get-time (:clock pkg) (timestamp)) msg))))
  (handle-next-event! apkg))

(comment

  (require '[pettomato.devs.atomic-simulator :refer [atomic-simulator]])
  (require '[pettomato.devs.models :refer [atomic-model]])

  (defn generator [period]
    (atomic-model
     {}
     identity
     (fn [s e x]
       (printf "%s received: %s\n" (timestamp) x)
       s)
     nil
     (fn [_] [[:out (timestamp)]])
     (constantly period)))

  (def pkg (-> (generator 1000)
               atomic-simulator
               (rt-root-simulator 0 println)))

  (schedule! pkg 0 "initial")

  (start! pkg)

  (schedule-now! pkg "test")

  (stop! pkg)

  )
