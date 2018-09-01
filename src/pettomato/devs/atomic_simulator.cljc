(ns pettomato.devs.atomic-simulator
  (:require
   [pettomato.devs.Simulator :refer [Simulator]]
   [pettomato.devs.models :refer [atomic? initial-state int-update-fn ext-update-fn con-update-fn output-fn time-advance-fn]]))

(defn- checked-time-advance [m s]
  (let [sigma ((time-advance-fn m) s)]
    (assert (>= sigma 0))
    sigma))

(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (init       [this t]
    (let [state' (initial-state model)]
      (AtomicSimulator. model state' t (+ t (checked-time-advance model state')))))
  (int-update [this t]
    (assert (= t tn) (str "(= " t " " tn ")"))
    (let [y      ((output-fn model) state)
          state' ((int-update-fn model) state)
          this'  (AtomicSimulator. model state' t (+ t (checked-time-advance model state')))]
      [this' y]))
  (ext-update [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [e      (- t tl)
          state' ((ext-update-fn model) state e x)]
      (AtomicSimulator. model state' t (+ t (checked-time-advance model state')))))
  (con-update [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [y      ((output-fn model) state)
          e      (- t tl)
          state' ((con-update-fn model) state e x)
          this'  (AtomicSimulator. model state' t (+ t (checked-time-advance model state')))]
      [this' y]))ount
  (tl         [this] tl)
  (tn         [this] tn))

(defn atomic-simulator [model]
  (assert (atomic? model))
  (AtomicSimulator. model nil nil nil))
