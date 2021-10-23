(ns pettomato.devs.root-coordinators.rt-afap-root-coordinator
  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.root-coordinators.rt-step-root-coordinator
    :refer [rt-step-root-coordinator step-through-to-wall-time get-sim-time]]))

(defn rt-afap-root-coordinator
  "Run a real-time simulation \"as fast as possible\".

  Args:
    sim - A real-time simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: 0.

    end - Simulation end time (exclusive). Default: infinity.

    step-size - The number of milliseconds of (fake) real-time to advance the
    simulation each step.

  Returns:
    A seq of [timestamp mail].

  This can be used to test real-time simulators and rt-step-root-coordinator,
  using the same interface as afap-root-coordinator and without having to wait
  for actual time to pass."
  [sim & {:keys [start end step-size]
          :or   {start     h/zero
                 end       h/infinity
                 step-size 100}}]
  (let [wall-time 0]
    (loop [rc        (rt-step-root-coordinator sim wall-time :start start)
           t         wall-time
           event-log []]
      (let [[rc' event-log'] (step-through-to-wall-time rc t :max-sim-time end)
            event-log        (into event-log event-log')]
       (if (h/< (get-sim-time rc') end)
         (recur rc' (+ t step-size) event-log)
         event-log)))))
