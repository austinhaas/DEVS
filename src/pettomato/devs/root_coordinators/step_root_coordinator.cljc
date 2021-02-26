(ns pettomato.devs.root-coordinators.step-root-coordinator
  (:require
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-next-event]]
   [pettomato.devs.vars :refer [*sim-time*]]))

(defn- step-1
  "Advance sim to time t. Returns [sim mail]."
  [sim t]
  (binding [*sim-time* t]
    (log/tracef "step-1 t: %s" t)
    (let [[sim out] (collect-mail sim t)
          sim       (transition sim {} t)]
      [sim out])))

(defn step-while
  "Advance sim while (compare (time-of-next-event sim) end) returns logical true.

  Returns [sim event-log]."
  [sim end & {:keys [compare]
              :or   {compare <}}]
  (log/tracef "step-while end: %s" end)
  (loop [sim sim
         out []]
    (let [t (time-of-next-event sim)]
      (cond
        (compare t end) (let [[sim out'] (step-1 sim t)
                              out        (if (seq out')
                                           (conj out [t out'])
                                           out)]
                          (recur sim out))
        :else           [sim out]))))

(defn step-to
  "Advance sim to end-time (exclusive).

  Returns [sim event-log]."
  [sim end]
  (step-while sim end :compare <))

(defn step-through
  "Advance sim to end time (inclusive).

  Returns [sim event-log].

  Note that this does not guarantee that there won't be another event at end
  time. Simulators only guarantee that timestamps are nondecreasing."
  [sim end]
  (step-while sim end :compare <=))

(defn step-root-coordinator
  "Step through a simulation.

  Args:
    sim - A simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: 0.

  Returns:
    An initialized simulator. Use `step-while`, `step-to`, and
   `step-through` to advance it."
  [sim & {:keys [start]
          :or   {start 0}}]
  (binding [*sim-time* start]
    (initialize sim start)))
