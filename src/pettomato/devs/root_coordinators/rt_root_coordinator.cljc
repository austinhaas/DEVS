(ns pettomato.devs.root-coordinators.rt-root-coordinator
  (:require
   [pettomato.devs.lib.clock :as clock]
   [pettomato.devs.lib.date :as date]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.simulator :as sim]))

(defn rt-root-coordinator
  "Run a simulation in real-time.

  Args:
    sim - A simulator.

  Optional keyword args:

    start - Simulation start time (inclusive). Default: (hyperreal)
  zero.

    wall-time-fn - A function of no args that returns the current
  wall-time. See `pettomato.devs.lib.clock` for more info.

  Returns:
    An opaque value that can be passed to `step` to advance the sim."
  [sim & {:keys [start wall-time-fn]
          :or   {start        h/zero
                 wall-time-fn date/timestamp}}]
  (let [clock (clock/clock (wall-time-fn) start)
        sim   (sim/initialize sim (clock/get-sim-time clock))]
    {:sim          sim
     :clock        clock
     :wall-time-fn wall-time-fn}))

(defn step
  "Step pkg to the current wall-time. Returns [pkg mail-log]."
  ([pkg]
   (let [{:keys [clock sim wall-time-fn]} pkg]
     (let [wt             (wall-time-fn)
           st             (clock/get-sim-time clock wt)
           [sim mail-log] (sim/step* sim st)]
       [(assoc pkg :sim sim) mail-log])))
  ([pkg mail]
   (let [{:keys [clock sim wall-time-fn]} pkg]
     (let [wt             (wall-time-fn)
           st             (clock/get-sim-time clock wt)
           [sim mail-log] (sim/step* sim st mail)]
       [(assoc pkg :sim sim) mail-log]))))

(defn set-scale-factor
  "This can be used to pause the simulation.

  See `pettomato.devs.lib.clock` for more info."
  [pkg scale-factor]
  (let [{:keys [wall-time-fn]} pkg]
    (update pkg :clock clock/set-scale-factor (wall-time-fn) scale-factor)))
