(ns pettomato.devs.root-coordinators.rt-step-root-coordinator
  (:require
   [pettomato.devs.lib.clock :as clock]
   [pettomato.devs.lib.event-log :refer [pp-event-log]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.root-coordinators.step-root-coordinator :refer [step-through step-root-coordinator]]
   [pettomato.devs.simulator :refer [transition time-of-last-event]]
   [pettomato.devs.vars :refer [*sim-time*]]))

(defn rt-step-root-coordinator
  "A real-time root-coordinator that can be run in steps.

  This provides a system to map from wallclock-time to sim-time, but something
  else still needs to drive the updates.

  Options:

  start - Simulation start time (inclusive). Default: 0.

  scale - The time scale factor. Default: 1.0.

  output-fn - A function that will be invoked each time there is simulation
  output. It should take a single argument: an event log."
  [sim wall-time & {:keys [start scale output-fn]
                    :or   {start     0
                           scale     1.0
                           output-fn (fn [event-log]
                                       (let [s (with-out-str (pp-event-log event-log))]
                                         (when (seq s)
                                           (log/infof "*** output *** \n%s" s))))}}]
  {:clock     (clock/clock wall-time start :scale-factor scale)
   :sim       (step-root-coordinator sim :start start)
   :output-fn output-fn
   :status    :stopped})

(defn- step-through-and-transition
  "Like step-through, but always invokes a transition at end time, even if the
  simulator is not imminent (presumably, for time synchronization)."
  [sim end]
  (log/tracef "step-through-and-transition end: %s" end)
  (let [[sim event-log] (step-through sim end)]
    ;; If there happens to be a scheduled event at end time, then there is no
    ;; need for the artificial transition. But if there isn't, the artificial
    ;; transition will be made, and then the sim will be step(ped)-through to
    ;; the current time again, in case the artificial transition resulted in the
    ;; sim becoming immediately imminent.
    (log/tracef "time-of-last-event: %s" (time-of-last-event sim))
    (if (= (time-of-last-event sim) end)
      [sim event-log]
      ;; No need to collect mail; we know the sim isn't imminent, or the
      ;; previous call to step-through would've captured that.
      (let [sim              (binding [*sim-time* end] (transition sim {} end))
            [sim event-log'] (step-through sim end)]
        [sim (concat event-log event-log')]))))

(defn step-through-to-sim-time
  [rc sim-time]
  (let [{:keys [clock sim output-fn]} rc
        end                           sim-time
        _                             (log/tracef "Updating sim-time to %s" end)
        [sim out]                     (step-through-and-transition sim end)]
    (output-fn out)
    (assoc rc :clock clock :sim sim)))

(defn step-through-to-wall-time
  [rc wall-time & {:keys [max-sim-time]
                   :or   {max-sim-time infinity}}]
  (let [{:keys [clock sim output-fn]} rc
        clock                         (clock/advance clock wall-time)
        sim-time                      (min (clock/get-sim-time clock) max-sim-time)
        _                             (log/tracef "Updating sim-time to %s" sim-time)
        [sim out]                     (step-through-and-transition sim sim-time)]
    (output-fn out)
    (assoc rc :clock clock :sim sim)))

(defn get-clock-scale-factor
  [rc]
  (clock/get-scale-factor (:clock rc)))

(defn set-clock-scale-factor
  [rc wt scale]
  (update rc :clock clock/set-scale-factor wt scale))

(defn get-sim-time [rc]
  (clock/get-sim-time (:clock rc)))
