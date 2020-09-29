(ns pettomato.devs.simulators.executive
  (:require
   [pettomato.devs.Simulator :refer [Simulator]]
   [pettomato.devs.ExecSimulator :refer [ExecSimulator]]
   [pettomato.devs.models.executive :refer [executive-model?]]))

(defrecord ExecutiveSimulator [model state tl tn]
  Simulator
  (receive-i-message [this t]
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance-fn model) s))]
      (ExecutiveSimulator. model s tl tn)))
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
                  :else              (throw (ex-info "error" {})))
          tl    t
          tn    (+ tl ((:time-advance-fn model) state))]
      (ExecutiveSimulator. model state tl tn)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn)
  ExecSimulator
  (get-network-structure [this] ((:network-fn model) state)))

(defn executive-simulator [model]
  (assert (executive-model? model))
  (ExecutiveSimulator. model nil nil nil))
