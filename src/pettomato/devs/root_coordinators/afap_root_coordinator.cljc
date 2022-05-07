(ns pettomato.devs.root-coordinators.afap-root-coordinator
  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-next-event]]))

(defn afap-root-coordinator
  "Run a simulation \"as fast as possible\".

  Args:
    sim - A simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: zero, in a hyperreal structure.
    end - Simulation end time (inclusive, unless infinity). Default: infinity.

  Returns:
    A lazy seq of [time mail], containing all messages sent to the root network.

  \"Analytic simulations typically execute 'as-fast-as-possible,' meaning that
  the simulation attempts to complete its computations as quickly as it
  can. This could mean that the simulator advances faster than real-time (for
  example, it might simulate hours of system behavior in only minutes of elapsed
  time to the user) or that it runs slower than real-time.\"

    - Fujimoto. Parallel and Distributed Simulation Systems. 2000. p. 7."
  [sim & {:keys [start end]
          :or   {start h/zero
                 end   h/infinity}}]
  (letfn [(step [sim t tl]
            (lazy-seq
             (let [tn (time-of-next-event sim)]
               (when (and tl (h/<= tn tl))
                 (throw (ex-info "NIA violation." {:tn tn :tl tl})))
               (when (h/< t start)
                 (throw (ex-info "t can't be before start." {:tn tn
                                                             :tl tl
                                                             :t  t
                                                             :start start})))
               (log/tracef "tn: %s" tn)
               (if (or (h/infinite? tn)
                       (h/< end tn))
                 nil
                 (let [mail (collect-mail sim tn)
                       sim' (transition sim nil tn)]
                   (if (seq mail)
                     (cons [tn mail] (step sim' tn t))
                     (step sim' tn t)))))))]
    (step (initialize sim start) start nil)))
