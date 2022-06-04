(ns pettomato.devs.root-coordinators.afap-root-coordinator
  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.simulator :as sim]))

(defn afap-root-coordinator
  "Run a simulation \"as fast as possible\".

  Args:
    sim - A simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: (hyperreal) zero.
    end - Simulation end time (inclusive, unless infinity). Default: (hyperreal) infinity.
    input-log - A seq of [sim-time mail] to be used as input to sim.

  Returns:
    mail-log

  \"Analytic simulations typically execute 'as-fast-as-possible,' meaning that
  the simulation attempts to complete its computations as quickly as it
  can. This could mean that the simulator advances faster than real-time (for
  example, it might simulate hours of system behavior in only minutes of elapsed
  time to the user) or that it runs slower than real-time.\"

    - Fujimoto. Parallel and Distributed Simulation Systems. 2000. p. 7."
  [sim & {:keys [start end input-log]
          :or   {start     h/zero
                 end       h/infinity
                 input-log []}}]
  (-> (sim/initialize sim start)
      (sim/step** :end end :mail-log input-log)
      second))
