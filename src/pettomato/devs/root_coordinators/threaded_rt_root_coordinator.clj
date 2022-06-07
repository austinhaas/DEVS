(ns pettomato.devs.root-coordinators.threaded-rt-root-coordinator
  (:require
   [pettomato.devs.lib.clock :as clock]
   [pettomato.devs.lib.date :as date]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :as mail]
   [pettomato.devs.simulator :as sim]))

(defn default-print-mail [mail-log]
  (binding [*print-level*  10
            *print-length* 10]
    (doseq [[t mail] mail-log]
      (log/infof "out> %s %s" t mail))))

(declare step-loop)

(def ^:private lock (Object.))

(defn rt-root-coordinator
  [sim & {:keys [sim-time wall-time-fn paused? scale-factor output-fn]
          :or   {sim-time     h/zero
                 wall-time-fn date/timestamp
                 paused?      false
                 scale-factor 1.0
                 output-fn    default-print-mail}}]
  (locking lock
    (let [clock  (clock/clock :sim-time sim-time
                              :paused? paused?
                              :scale-factor scale-factor
                              :wall-time-fn wall-time-fn)
          sim    (sim/initialize sim (clock/get-sim-time clock))
          pkg    (atom
                  {:clock     clock
                   :sim       sim
                   :output-fn output-fn})]
      (if paused?
        pkg
        (step-loop pkg)))))

(defn- wall-time-until-next-event [pkg]
  (let [{:keys [clock sim]} @pkg
        next-sim-time       (sim/time-of-next-event sim)
        curr-sim-time       (clock/get-sim-time clock)
        scale-factor        (clock/get-scale-factor clock)]
    (if (h/< curr-sim-time next-sim-time)
      (let [delta (h/- next-sim-time curr-sim-time)]
        (if (h/infinite? delta)
          (h/standard delta)
          (/ (h/standard delta) scale-factor)))
      0)))

(defn- step!
  ([pkg]
   (let [{:keys [clock sim output-fn]} @pkg
         [sim mail] (sim/step* sim (clock/get-sim-time clock))]
     (swap! pkg assoc :sim sim)
     (output-fn mail)
     pkg))
  ([pkg mail]
   (let [{:keys [clock sim output-fn]} @pkg
         [sim mail] (sim/step* sim (clock/get-sim-time clock) mail)]
     (swap! pkg assoc :sim sim)
     (output-fn mail)
     pkg)))

(defn- step-loop
  [pkg]
  (let [t (Thread.
           (fn []
             (try
               (while (and (not (Thread/interrupted))
                           (not (h/infinite? (sim/time-of-next-event (:sim @pkg)))))
                 (Thread/sleep (wall-time-until-next-event pkg))
                 (step! pkg))
               (catch java.lang.InterruptedException e
                 nil))))]
    (swap! pkg assoc :thread t)
    (.start t)
    pkg))

(defn paused? [pkg]
  (-> pkg deref :clock clock/paused?))

(defn pause! [pkg]
  (locking lock
    (when (not (paused? pkg))
      (swap! pkg update :clock clock/pause)
      (.interrupt (:thread @pkg))
      (.join (:thread @pkg)))
    pkg))

(defn unpause! [pkg]
  (locking lock
    (when (paused? pkg)
      (swap! pkg update :clock clock/unpause)
      (step-loop pkg))
    pkg))

(defn send-mail! [pkg mail]
  (locking lock
    (.interrupt (:thread @pkg))
    (.join (:thread @pkg))
    (step! pkg mail)
    (if (paused? pkg)
      pkg
      (step-loop pkg))))

(defn set-scale-factor! [pkg scale-factor]
  (locking lock
    (.interrupt (:thread @pkg))
    (.join (:thread @pkg))
    (swap! pkg update :clock clock/set-scale-factor scale-factor)
    (if (paused? pkg)
      pkg
      (step-loop pkg))))
