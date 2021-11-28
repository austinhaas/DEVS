(ns pettomato.devs.simulators.atomic-simulator
  "A simulator for atomic models."
  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [Simulator]]))

(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    (let [[s e] (:initial-total-state model)
          tl    (h/- t e)
          ta    ((:time-advance model) s)
          _     (assert (h/pos? ta) "time-advance must be positive")
          tn    (h/+ tl ta)]
      (assoc sim :state s :tl tl :tn tn)))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (assert (h/= t tn) (str "synchronization error: (not (= " t " " tn "))"))
    (let [mail      ((:output    model) state)
          petitions ((:petitions model) state)]
      [sim mail petitions]))
  (transition [sim mail t]
    (log/tracef "--- transition ---")
    (assert (h/<= tl t tn) (str "synchronization error: (not (<= " tl " " t " " tn "))"))
    (let [state (cond
                  (and (h/= t tn) (empty? mail)) ((:internal-update  model) state)
                  (and (h/= t tn) (seq    mail)) ((:confluent-update model) state mail)
                  (and (h/< t tn) (seq    mail)) ((:external-update  model) state (h/- t tl) mail)
                  :else                          (throw (ex-info "Illegal state for transition; sim is not imminent nor receiving mail."
                                                                 {:tl         tl
                                                                  :t          t
                                                                  :tn         tn
                                                                  :mail-count (count mail)})))
          tl    t
          ta    ((:time-advance model) state)
          _     (assert (h/pos? ta) "time-advance must be positive")
          tn    (h/+ tl ta)]
      (assoc sim :state state :tl tl :tn tn)))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(defn atomic-simulator
  "Wrap an atomic model in an atomic simulator.

  Args:
    model - An atomic model.

  Returns:
    A simulator."
  [model]
  (map->AtomicSimulator {:model model}))
