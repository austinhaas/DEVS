(ns pettomato.devs.root-coordinators.afap-root-coordinator
  (:require
   [pettomato.devs.lib.event-log :refer [format-event-log]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :refer [log-fn]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-next-event]]
   [pettomato.devs.vars :refer [*sim-time*]]))

(defn afap-root-coordinator
  "Run a simulation \"as fast as possible\".

  Returns a seq of [timestamp mail].

  Optional keyword args:

    start - Simulation start time (inclusive). Default: 0.

    end - Simulation end time (exclusive). Default: infinity.

    max-steps - Maximum number of steps. Intended usage: preventing runaway
  simulations (e.g., a model constantly returns a time-advance value of
  0). Default: infinity. Throws an AssertionError if max-steps is reached."
  [sim & {:keys [start end max-steps]
          :or   {start     0
                 end       infinity
                 max-steps infinity}}]
  (log/tracef "START afap-root-coordinator {:start %s :end %s :max-steps %s}" start end max-steps)
  (loop [sim (binding [*sim-time* start] (initialize sim start))
         out (transient [])
         i   0]
    (assert (< i max-steps) (str "Maximum number of steps reached: " i))
    (let [t (time-of-next-event sim)]
      (binding [*sim-time* t] (log/tracef "[ step %s ] --------------------------------------------------" i))
      (if (< t end)
        (let [[sim out'] (binding [*sim-time* t] (collect-mail sim t))
              sim        (binding [*sim-time* t] (transition sim {} t))
              out        (if (seq out')
                           (conj! out [t out'])
                           out)]
          (recur sim out (inc i)))
        (do (log/tracef "END afap-root-coordinator {:start %s :end %s :max-steps %s}" start end max-steps)
            (-> (persistent! out)
                format-event-log))))))
