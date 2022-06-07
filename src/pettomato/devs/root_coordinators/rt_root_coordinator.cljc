(ns pettomato.devs.root-coordinators.rt-root-coordinator
  (:require
   [pettomato.devs.lib.clock :as clock]
   [pettomato.devs.simulator :as sim]))

(defn rt-root-coordinator
  "Run a simulation in real-time.

  Args:

    sim - A simulator.

    clock - A clock.

  Returns:
    A simulator initialized based on clock."
  [sim clock]
  (sim/initialize sim (clock/get-sim-time clock)))

(defn step
  "Step sim based on clock. Returns [sim mail-log]."
  ([sim clock]
   (sim/step* sim (clock/get-sim-time clock)))
  ([sim clock mail]
   (sim/step* sim (clock/get-sim-time clock) mail)))
