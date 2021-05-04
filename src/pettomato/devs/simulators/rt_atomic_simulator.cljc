(ns pettomato.devs.simulators.rt-atomic-simulator
  "A real-time atomic simulator.

  Differences from the non-real-time version:

    1. This version does not cache time-of-next-event. This allows the simulator
  to change its answer over time based on events outside the simulation (such as
  a human-in-the-loop that returns infinity, until it makes a decision, and then
  returns a finite number).

    2. This version handles the case where the transition function is invoked
  and the simulator is not immiment nor receiving mail. These \"no-op\"
  invocations are used for real-time synchronization; they tell the simulator
  that it can advance to the current sim-time."
  (:require
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [Simulator time-of-next-event]]))

(defrecord RealTimeAtomicSimulator [model state tl]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    (let [[s e] (:initial-total-state model)
          tl    (- t e)]
      (assoc sim :state s :tl tl)))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (let [tn (time-of-next-event sim)]
      (assert (= t tn) (str "synchronization error: (not (= " t " " tn "))")))
    (let [mail ((:output model) state)]
      [sim mail]))
  (transition [sim mail t]
    (log/trace "--- transition ---")
    (let [tn (time-of-next-event sim)]
      (assert (<= tl t tn) (str "synchronization error: (not (<= " tl " " t " " tn "))"))
      (let [state (if (= t tn)
                    (if (seq mail)
                      ((:confluent-update model) state mail)
                      ((:internal-update  model) state))
                    ((:external-update  model) state (- t tl) mail))
            tl    t]
        (assoc sim :state state :tl tl))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] (+ tl ((:time-advance model) state))))

(defn rt-atomic-simulator
  "Wrap a real-time atomic model in a real-time atomic simulator.

  Args:
    model - An real-time atomic model.

  Returns:
    A simulator.

  The only difference between an atomic model and a real-time atomic model is
  that the real-time model's external-update function must handle being invoked
  with an empty set of incoming messages. Since external-updates are passed the
  elapsed-time, these \"no-op\" invocations can be used for temporal
  synchronization. The function `pettomato.devs.models.atomic-model/->rt` can be
  used to automically convert non-real-time atomic models to real-time atomic
  models."
  [model]
  (map->RealTimeAtomicSimulator {:model model}))
