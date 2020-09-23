(ns pettomato.devs.atomic-simulator
  (:require
   [clojure.spec.alpha :as s]
   [pettomato.devs.Simulator :refer [Simulator]]
   [pettomato.devs.models :as m]))

;; TODO: Consider passing the model fns instead of the model.
(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (receive-i-message [this t]
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance model) s))]
      (AtomicSimulator. model s tl tn)))
  (receive-*-message [this t]
    (assert (= t tn) (str "(= " t " " tn ")"))
    [this ((:output model) state)])
  (receive-x-message [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [state (cond
                  (and (empty? x)
                       (= t tn))     ((:int-update model) state)
                  (and (seq x)
                       (= t tn))     ((:con-update model) state x)
                  (and (seq x)
                       (<= tl t tn)) ((:ext-update model) state (- t tl) x)
                  :else              (throw (ex-info "error" {})))
          tl    t
          tn    (+ tl ((:time-advance model) state))]
      (AtomicSimulator. model state tl tn)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn)
  #_
  (apply-state-changes [this state-changes]
    ((:apply-state-changes model) state-changes)))

(defn atomic-simulator [model]
  (assert (s/valid? ::m/atomic-model model)
          (s/explain-str ::m/atomic-model model))
  (AtomicSimulator. model nil nil nil))
