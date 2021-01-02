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

  Options:

  start - Simulation start time (inclusive). Default: 0.

  end - Simulation end time (exclusive). Default: infinity.

  limit - Maximum number of iterations. Intended to prevent runaway
  simulations. Default: infinity."
  [sim & {:keys [start end limit]
          :or   {start 0
                 end   infinity
                 limit infinity}}]
  (binding [log/*log-function* log-fn]
    (log/infof "START afap-root-coordinator {:start %s :end %s :limit %s}" start end limit)
    (loop [sim (binding [*sim-time* start] (initialize sim start))
           out (transient [])
           i   0]
      (assert (< i limit) (str "limit reached: " i))
      (let [t (time-of-next-event sim)]
        (binding [*sim-time* t] (log/tracef "[ step %s ] --------------------------------------------------" i))
        (if (< t end)
          (let [[sim out'] (binding [*sim-time* t] (collect-mail sim t))
                sim        (binding [*sim-time* t] (transition sim {} t))
                out        (if (seq out')
                             (conj! out [t out'])
                             out)]
            (recur sim out (inc i)))
          (do (log/infof "END afap-root-coordinator {:start %s :end %s :limit %s}" start end limit)
              (-> (persistent! out)
                  format-event-log)))))))
