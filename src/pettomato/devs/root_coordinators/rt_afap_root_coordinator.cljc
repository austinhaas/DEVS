(ns pettomato.devs.root-coordinators.rt-afap-root-coordinator
  (:require
   [pettomato.devs.lib.number :refer [infinity]]
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

  This can be used to test real-time simulators and the rt-step-root-coordinator
  with the same interface as afap-root-coordinator and without having to wait
  for actual time to pass."
  [sim & {:keys [start end step-size]
          :or   {start     0
                 end       infinity
                 step-size 100}}]
  (let [output (atom [])
        rc     (rt-step-root-coordinator
                sim
                0
                :start start
                :output-fn (fn [event-log]
                             (when (seq event-log)
                               (swap! output into event-log))))]
    (loop [rc rc
           t  0]
      (let [rc' (step-through-to-wall-time rc t :max-sim-time end)]
        (when (< (get-sim-time rc') end)
          (recur rc' (+ t step-size)))))
    @output))
