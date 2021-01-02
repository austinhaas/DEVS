(ns pettomato.devs.simulators.atomic-simulator
  (:require
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [Simulator]]))

(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance model) s))]
      (assoc sim :state s :tl tl :tn tn)))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (assert (= t tn) (str "synchronization error: (not (= " t " " tn ")"))
    (let [mail ((:output model) state)]
      [sim mail]))
  (transition [sim mail t]
    (log/trace "--- transition ---")
    (assert (<= tl t tn) (str "synchronization error: (not (<= " tl " " t " " tn ")"))
    (let [state (cond
                  (and (= t tn) (empty? mail)) ((:internal-update  model) state)
                  (and (= t tn) (seq    mail)) ((:confluent-update model) state mail)
                  (< t tn)                     ((:external-update  model) state (- t tl) mail)
                  :else                        (throw (ex-info "unexpected state" {})))
          tl    t
          tn    (+ tl ((:time-advance model) state))]
      (assoc sim :state state :tl tl :tn tn)))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(defn atomic-simulator [model]
  (map->AtomicSimulator {:model model}))
