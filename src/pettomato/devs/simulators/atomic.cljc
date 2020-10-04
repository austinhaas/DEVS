(ns pettomato.devs.simulators.atomic
  (:require
   [pettomato.devs.Simulator :refer [Simulator]]
   [pettomato.devs.models.atomic :refer [atomic-model?]]
   [pettomato.devs.util :refer [trace]]))

(defrecord AtomicSimulator [id model state tl tn]
  Simulator
  (receive-i-message [this t]
    (trace "[%s] {%s} ************************************" t id)
    (trace "[%s] {%s} AtomicSimulator/receive-i-message" t id)
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance-fn model) s))]
      (AtomicSimulator. id model s tl tn)))
  (receive-*-message [this t]
    (trace "[%s] {%s} ====================================" t id)
    (trace "[%s] {%s} AtomicSimulator/receive-*-message" t id)
    (when-not (= t tn)
      (throw (ex-info (str "synchronization error" " (not (= " t " " tn "))")
                      {:t t :tn tn :tl tl})))
    [this ((:output-fn model) state)])
  (receive-x-message [this x t]
    (trace "[%s] {%s} ====================================" t id)
    (trace "[%s] {%s} NetworkSimulator/receive-x-message: %s" t id x)
    (when-not (<= tl t tn)
      (throw (ex-info (str "synchronization error" " (not (<= " tl " " t " " tn "))")
                      {:t t :tn tn :tl tl})))
    (let [state (cond
                  (and (empty? x)
                       (= t tn))     ((:internal-update-fn model) state)
                  (and (seq x)
                       (= t tn))     ((:confluent-update-fn model) state x)
                  (and (seq x)
                       (<= tl t tn)) ((:external-update-fn model) state (- t tl) x)
                  :else              (throw (ex-info "Can't determine which state transition function to call."
                                                     {:x x :tl tl :t t :tn tn})))
          tl    t
          tn    (+ tl ((:time-advance-fn model) state))]
      (AtomicSimulator. id model state tl tn)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn))

(defn atomic-simulator [id model]
  (assert (atomic-model? model))
  (AtomicSimulator. id model nil nil nil))
