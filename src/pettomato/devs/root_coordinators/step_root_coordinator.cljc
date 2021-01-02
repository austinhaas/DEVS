(ns pettomato.devs.root-coordinators.step-root-coordinator
  "A root coordinator that can be run in steps."
  (:require
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :refer [log-fn]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-next-event]]
   [pettomato.devs.vars :refer [*sim-time*]]))

(defn- step-1 [sim t]
  (binding [*sim-time* t]
    (log/tracef "step-1 t: %s" t)
    (let [[sim out] (collect-mail sim t)
          sim       (transition sim {} t)]
      [sim out])))

(defn step-while
  "Step sim while (compare (time-of-next-event sim) end) returns logical true.

  Returns [sim event-log]."
  [sim end & {:keys [compare]
              :or   {compare <}}]
  (log/tracef "step-while end: %s" end)
  (loop [sim sim
         out []]
    (let [t (time-of-next-event sim)]
      (cond
        (compare t end) (let [[sim out'] (step-1 sim t)
                              out        (conj out [t out'])]
                          (recur sim out))
        :else           [sim out]))))

(defn step-to
  "Step sim up to, but not including, time end.

  Returns [sim event-log]."
  [sim end]
  (step-while sim end :compare <))

(defn step-through
  "Step sim up to, and including, end time.

  Note that this doesn't guarantee that there won't be another event at end
  time. In general, simulators only guarantee that the timestamps are
  nondecreasing, and the real-time simulator, in particular, allows simulators
  to change their time-of-next-event.

  Returns [sim event-log]."
  [sim end]
  (step-while sim end :compare <=))

(defn step-root-coordinator
  [sim & {:keys [start]
          :or   {start 0}}]
  (binding [log/*log-function* log-fn
            *sim-time*         start]
    (initialize sim start)))
