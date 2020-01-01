(ns pettomato.devs.rt-root-simulator-base
  "Like root-simulator-base, but includes the concept of wall-time,
  which is used to determine the size of the step in simulation time.

  This ns provides basic support for a real-time simulator, but leaves
  the platform and application-specific issues, such as determining
  the current wall time, scheduling regular updates, and the
  suspension and resumption of a simulation, to extensions."
  (:require
   [pettomato.devs.root-simulator-base :as rsb]
   [pettomato.lib.log :as log]))

;; This shouldn't include max-delta. That is application-specific.

;; I'm not sure this ns is valuable as a core part of this library.

(defn rt-root-simulator [sim start-time max-delta]
  (log/info "rt-root-simulator")
  {:root-sim  (rsb/root-simulator sim start-time)
   :max-delta max-delta})

(defn- calc-sim-time [pkg curr-wall-time]
  ;;(log/infof "calc-sim-time: %s" curr-wall-time)
  (let [prev-wall-time (or (:prev-wall-time pkg) curr-wall-time)
        ;;_ (log/infof "prev-wall-time: %s" prev-wall-time)
        wall-delta     (- curr-wall-time prev-wall-time)
        ;;_ (log/infof "wall-delta: %s" wall-delta)
        _              (assert (<= 0 wall-delta) "curr-wall-time cannot be less than prev-wall-time.")
        max-delta      (:max-delta pkg)
        sim-delta      (min wall-delta max-delta)
        ;;_ (log/infof "sim-delta: %s" sim-delta)
        prev-sim-time  (rsb/time-of-last-update (:root-sim pkg))
        ;;_ (log/infof "prev-sim-time: %s" prev-sim-time)
        curr-sim-time  (+ prev-sim-time sim-delta)]
    ;;(log/infof "curr-sim-time: %s" curr-sim-time)
    curr-sim-time))

(defn advance-now
  [pkg curr-wall-time]
  (let [curr-sim-time        (calc-sim-time pkg curr-wall-time)
        ;;_   (log/infof "advance-now: %s [%s]" curr-wall-time curr-sim-time)
        root-sim             (:root-sim pkg)
        [root-sim' tmsg-out] (rsb/advance root-sim curr-sim-time)]
    [(assoc pkg
             :prev-wall-time curr-wall-time
             :root-sim       root-sim')
     tmsg-out]))

(defn schedule [pkg t msg]
  (update pkg :root-sim rsb/schedule t msg))

(defn schedule* [pkg tmsgs]
  (update pkg :root-sim rsb/schedule* tmsgs))

(defn schedule-now [pkg curr-wall-time msg]
  (let [curr-sim-time (calc-sim-time pkg curr-wall-time)]
    (log/infof "schedule-now: %s [%s]" curr-wall-time curr-sim-time)
    (update pkg :root-sim rsb/schedule curr-sim-time msg)))
