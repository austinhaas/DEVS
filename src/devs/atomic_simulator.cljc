(ns devs.atomic-simulator
  (:require
   [devs.Simulator :refer [Simulator]]
   [devs.atomic-model :refer [atomic?]]))

(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (init       [this t]
    (let [state' (:initial-state model)]
      (AtomicSimulator. model state' t (+ t ((:time-advance-fn model) state')))))
  (int-update [this t]
    (assert (= t tn) (format "(= %s %s)" t tn))
    (let [y      ((:output-fn model) state)
          state' ((:int-update-fn model) state)
          this'  (AtomicSimulator. model state' t (+ t ((:time-advance-fn model) state')))]
      [this' y]))
  (ext-update [this x t]
    (assert (<= tl t tn) (format "(<= %s %s %s)" tl t tn))
    (let [e      (- t tl)
          state' ((:ext-update-fn model) state e x)]
      (AtomicSimulator. model state' t (+ t ((:time-advance-fn model) state')))))
  (tl         [this] tl)
  (tn         [this] tn))

(defn atomic-simulator [model]
  (assert (atomic? model))
  (AtomicSimulator. model nil nil nil))
