(ns pettomato.devs.parallel.simulators.atomic
  (:require
   [pettomato.devs.parallel.Simulator :refer [Simulator]]
   [pettomato.devs.parallel.models.atomic :refer [atomic-model?
                                                  initial-state
                                                  int-update-fn
                                                  ext-update-fn
                                                  con-update-fn
                                                  output-fn
                                                  time-advance-fn]]))

;; TODO: Consider passing the model fns instead of the model.
(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (receive-i-message [this t]
    (let [[s e] (initial-state model)
          tl    (- t e)
          tn    (+ tl ((time-advance-fn model) s))]
      (AtomicSimulator. model s tl tn)))
  (receive-*-message [this t]
    (assert (= t tn) (str "(= " t " " tn ")"))
    [this ((output-fn model) state)])
  (receive-x-message [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [state (cond
                  (and (empty? x)
                       (= t tn))     ((int-update-fn model) state)
                  (and (seq x)
                       (= t tn))     ((con-update-fn model) state x)
                  (and (seq x)
                       (<= tl t tn)) ((ext-update-fn model) state (- t tl) x)
                  :else              (throw (ex-info "error" {})))
          tl    t
          tn    (+ tl ((time-advance-fn model) state))]
      (AtomicSimulator. model state tl tn)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn))

(defn atomic-simulator [model]
  (assert (atomic-model? model))
  (AtomicSimulator. model nil nil nil))
