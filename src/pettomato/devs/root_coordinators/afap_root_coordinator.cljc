(ns pettomato.devs.root-coordinators.afap-root-coordinator
  (:require
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.root-coordinators.step-root-coordinator
    :refer [step-root-coordinator step-through]]))

(defn afap-root-coordinator
  "Run a simulation \"as fast as possible\".

  Args:
    sim - A simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: 0.
    end - Simulation end time (inclusive, unless infinity). Default: infinity.

  Returns:
    A seq of [timestamp mail], containing all messages sent to the root network.

  \"Analytic simulations typically execute 'as-fast-as-possible,' meaning that
  the simulation attempts to complete its computations as quickly as it
  can. This could mean that the simulator advances faster than real-time (for
  example, it might simulate hours of system behavior in only minutes of elapsed
  time to the user) or that it runs slower than real-time.\"

    - Fujimoto. Parallel and Distributed Simulation Systems. 2000. p. 7."
  [sim & {:keys [start end]
          :or   {start 0
                 end   infinity}}]
  (let [[sim event-log] (-> (step-root-coordinator sim :start start)
                            (step-through end))]
    event-log))
