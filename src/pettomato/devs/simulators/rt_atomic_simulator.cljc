(ns pettomato.devs.simulators.rt-atomic-simulator
  "A real-time atomic simulator.

  The difference between this simulator and the regular atomic simulator is that
  it doesn't cache time-of-next-event."
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
      (assert (= t tn) (str "synchronization error: (not (= " t " " tn ")")))
    (let [mail ((:output model) state)]
      [sim mail]))
  (transition [sim mail t]
    (log/trace "--- transition ---")
    (let [tn (time-of-next-event sim)]
      (assert (<= tl t tn) (str "synchronization error: (not (<= " tl " " t " " tn ")"))
      (let [state (cond
                    (and (= t tn) (empty? mail)) ((:internal-update  model) state)
                    (and (= t tn) (seq    mail)) ((:confluent-update model) state mail)
                    (< t tn)                     ((:external-update  model) state (- t tl) mail)
                    :else                        (throw (ex-info "unexpected state" {})))
            tl    t]
        (assoc sim :state state :tl tl))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] (+ tl ((:time-advance model) state))))

(defn rt-atomic-simulator [model]
  (map->RealTimeAtomicSimulator {:model model}))
