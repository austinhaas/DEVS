(ns pettomato.devs.simulators.atomic
  (:require
   [pettomato.devs.Simulator :refer [Simulator]]
   [pettomato.devs.models.atomic :refer [atomic-model?]]))

(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (receive-i-message [this t]
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance-fn model) s))]
      (AtomicSimulator. model s tl tn)))
  (receive-*-message [this t]
    (assert (= t tn) (str "(= " t " " tn ")"))
    [this ((:output-fn model) state)])
  (receive-x-message [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [state (cond
                  (and (empty? x)
                       (= t tn))     ((:internal-update-fn model) state)
                  (and (seq x)
                       (= t tn))     ((:confluent-update-fn model) state x)
                  (and (seq x)
                       (<= tl t tn)) ((:external-update-fn model) state (- t tl) x)
                  :else              (throw (ex-info "Can't determine which state transition function to call."
                                                     {:x  x
                                                      :tl tl
                                                      :t  t
                                                      :tn tn})))
          tl    t
          tn    (+ tl ((:time-advance-fn model) state))]
      (AtomicSimulator. model state tl tn)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn))

(defn atomic-simulator [model]
  (assert (atomic-model? model))
  (AtomicSimulator. model nil nil nil))
