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

  This extends step-root-coordinator with functionality to map from wall-time to
  sim-time. It is still a \"step\" simulator; something else will need to drive
  the updates.

  Args:
    sim - A simulator.

    wall-time - The current wall-time.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: 0.

    scale - The time scale factor. Default: 1.0.

  Returns:
    A root-coordinator."
  [sim wall-time & {:keys [start scale-factor]
                    :or   {start        0
                           scale-factor 1.0}}]
  {:clock (clock/clock wall-time start :scale-factor scale-factor)
   :sim   (step-root-coordinator sim :start start)})

(defn- step-through-and-transition
  "Like step-through, but always invokes a transition at end time, even if the
  simulator is not imminent (presumably, for time synchronization)."
  [sim end]
  ;;(log/tracef "rt-step-root-coordinator/step-through-and-transition end: %s" end)
  (let [[sim event-log] (step-through sim end)]
    ;; If there happens to be a scheduled event at end time, then there is no
    ;; need for the artificial transition. But if there isn't, the artificial
    ;; transition will be made, and then the sim will be advanced to the current
    ;; time again, in case the artificial transition caused the sim to become
    ;; imminent.
    (if (= (time-of-last-event sim) end)
      [sim event-log]
      ;; No need to collect mail; we know the sim isn't imminent, or the
      ;; previous call to step-through would've caught that.
      (let [sim              (transition sim {} end)
            [sim event-log'] (step-through sim end)]
        [sim (concat event-log event-log')]))))

(defn step-through-to-wall-time
  "Advance the simulation to wall-time.

  Args:
    rc - A root coordinator.
    wall-time - The current wall-time.

  Optional keyword args:
    max-sim-time - The maximum sim-time the simulation should advance to (inclusive).
  Default: infinity.

  Returns:
    [rc event-log]"
  [rc wall-time & {:keys [max-sim-time]
                   :or   {max-sim-time infinity}}]
  ;;(log/tracef "rt-step-root-coordinator/step-through-to-wall-time: %s" wall-time max-sim-time)
  (let [{:keys [clock sim]} rc
        clock    (clock/advance clock wall-time)
        sim-time (min (clock/get-sim-time clock) max-sim-time)]
    (binding [*sim-time* sim-time]
      (let [[sim event-log] (step-through-and-transition sim sim-time)]
        [(assoc rc :clock clock :sim sim) event-log]))))

(defn get-clock-scale-factor
  "Returns the root coordinator's current time scale factor."
  [rc]
  (clock/get-scale-factor (:clock rc)))

(defn set-clock-scale-factor
  "Set the root coordinator's current time scale factor."
  [rc wall-time scale-factor]
  (update rc :clock clock/set-scale-factor wall-time scale-factor))

(defn get-sim-time
  "Returns the root coordinator's current sim-time."
  [rc]
  (clock/get-sim-time (:clock rc)))
