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
    (let [[sim out] (collect-mail sim t)
          sim       (transition sim {} t)]
      [sim out])))

(defn step-while
  "Advance sim while (compare (time-of-next-event sim) end) returns logical true.

  Returns [sim event-log]."
  ;; This could be generalized, with a user-supplied predicate that takes the
  ;; current sim or time-of-next-event, but I think this interface services the
  ;; overwhelming majority of use-cases. Also, a predicate that takes the
  ;; current sim would have to call time-of-next-event twice to implement this
  ;; use-case, since the predicate would be opaque and time-of-next-event is
  ;; still needed to make an update.
  [sim end & {:keys [compare]
              :or   {compare <}}]
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

;; Commented out because it isn't currently used, but may be useful in the
;; future.
#_
(defn step-to
  "Advance sim to end-time (exclusive).

  Returns [sim event-log]."
  [sim end]
  (step-while sim end :compare <))

(defn step-through
  "Advance sim to end time (inclusive, unless end = infinity).

  Returns [sim event-log].

  Note that this does not guarantee that there won't be another event at end
  time. Simulators only guarantee that timestamps are nondecreasing."
  [sim end]
  (if (= end infinity)
    (step-while sim end :compare <)
    (step-while sim end :compare <=)))

(defn step-root-coordinator
  "Step through a simulation.

  Args:
    sim - A simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: 0.

  Returns:
    An initialized simulator. Use `step-while`, `step-to`, and
   `step-through` to advance it.

  This may be thought of as the core of a root-coordinator. The key component
  that is missing is something to drive the updates. A complete root-coordinator
  implementation depends on the platform and requirements.

  Some examples:

    - An \"as-fast-as-possible\" simulator may run through an entire simulation
      in a single loop.

    - A real-time simulator might synchronize the updates with a real-time clock.

    - In the browser, a real-time simulator might use setInterval or
      updateAnimationFrame to drive the updates.

    - On the JVM, a real-time simulator might use threads.

    - A manually-driven system, perhaps for debugging, might provide an
      interface for users to advance the simulation one step at a time.

  All cases need the same logic for determining the next event and processing
  it."
  [sim & {:keys [start]
          :or   {start 0}}]
  (binding [*sim-time* start]
    (initialize sim start)))
