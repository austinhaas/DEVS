(ns pettomato.devs.root-coordinators.afap-root-coordinator
  (:require
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.root-coordinators.step-root-coordinator
    :refer [step-root-coordinator step-to]]))

(defn afap-root-coordinator
  "Run a simulation \"as fast as possible\".

  Args:
    sim - A simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: 0.
    end - Simulation end time (exclusive). Default: infinity.

  Returns:
    A seq of [timestamp mail]."
  [sim & {:keys [start end]
          :or   {start     0
                 end       infinity}}]
  (let [[sim event-log] (-> (step-root-coordinator sim :start start)
                            (step-to end))]
    event-log))
