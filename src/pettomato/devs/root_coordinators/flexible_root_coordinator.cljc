(ns pettomato.devs.root-coordinators.flexible-root-coordinator
  (:require
   [pettomato.devs.lib.clock :as clock]
   [pettomato.devs.lib.date :as date]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :as sim]))

;; Locking isn't necessary for CLJS (does it do anything at all?), but
;; it doesn't hurt either (AFAICT), so it's simpler to leave it in.

(def lock #?(:clj (Object.) :cljs (js/Object)))

(defn default-print-mail [mail-log]
  (binding [*print-level*  10
            *print-length* 10]
    (doseq [[t mail] mail-log]
      (log/infof "out> %s %s" t mail))))

(declare start-loop!)

(defn flexible-root-coordinator
  [sim & {:keys [sim-time
                 wall-time-fn
                 paused?
                 scale-factor
                 output-fn
                 start-loop-fn
                 stop-loop-fn]
          :or   {sim-time     h/zero
                 wall-time-fn date/timestamp
                 paused?      false
                 scale-factor 1.0
                 output-fn    default-print-mail}}]
  (let [clock (clock/clock :sim-time sim-time
                           :paused? paused?
                           :scale-factor scale-factor
                           :wall-time-fn wall-time-fn)
        sim   (sim/initialize sim (clock/get-sim-time clock))
        pkg   (atom
               {:clock         clock
                :sim           sim
                :output-fn     output-fn
                :start-loop-fn start-loop-fn
                :stop-loop-fn  stop-loop-fn
                :loop-handle   nil})]
    (if paused?
      pkg
      (start-loop! pkg))))

(defn- start-loop! [pkg]
  (swap! pkg assoc :loop-handle ((:start-loop-fn @pkg) pkg))
  pkg)

(defn- stop-loop! [pkg]
  (swap! pkg assoc :loop-handle ((:stop-loop-fn @pkg) (:loop-handle @pkg)))
  pkg)

(defn wall-time-until-next-event [pkg]
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

(defn step!
  ([pkg]
   (locking lock
     (let [{:keys [clock sim output-fn]} @pkg
           [sim mail] (sim/step* sim (clock/get-sim-time clock))]
       (swap! pkg assoc :sim sim)
       (output-fn mail)
       pkg)))
  ([pkg mail]
   (locking lock
     (let [{:keys [clock sim output-fn]} @pkg
           [sim mail] (sim/step* sim (clock/get-sim-time clock) mail)]
       (swap! pkg assoc :sim sim)
       (output-fn mail)
       pkg))))

(defn paused? [pkg]
  (-> pkg deref :clock clock/paused?))

(defn pause! [pkg]
  (locking lock
    (when (not (paused? pkg))
      (swap! pkg update :clock clock/pause)
      (stop-loop! pkg))
    pkg))

(defn unpause! [pkg]
  (locking lock
    (when (paused? pkg)
      (swap! pkg update :clock clock/unpause)
      (start-loop! pkg))
    pkg))

(defn send-mail! [pkg mail]
  (locking lock
    (stop-loop! pkg)
    (step! pkg mail)
    (when (not (paused? pkg))
      (start-loop! pkg))
    pkg))

(defn set-scale-factor! [pkg scale-factor]
  (locking lock
    (stop-loop! pkg)
    (swap! pkg update :clock clock/set-scale-factor scale-factor)
    (when (not (paused? pkg))
      (start-loop! pkg))
    pkg))
