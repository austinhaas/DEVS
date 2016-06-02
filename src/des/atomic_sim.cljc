(ns des.atomic-sim
  (:require
   [pt-lib.number :refer [infinity]]
   [pt-lib.coll :refer [group dissoc-in]]
   [des.model :refer [atomic?]]))

(defn atomic-sim [model]
  (assert (atomic? model))
  {:model model
   :state nil
   :tl    nil
   :tn    nil})

(defn init [sim t]
  (let [model (:model sim)
        state (:initial-state model)]
   (assoc sim
          :state state
          :tl    t
          :tn    (+ t ((:time-advance-fn model) state)))))

(defn int-update [sim t]
  (assert (= t (:tn sim)) (format "(= %s %s)" t (:tn sim)))
  (let [model  (:model sim)
        y      ((:output-fn model) (:state sim))
        state' ((:int-update-fn model) (:state sim))
        sim'   (assoc sim
                      :state state'
                      :tl    t
                      :tn    (+ t ((:time-advance-fn model) state')))]
    [sim' y]))

(defn ext-update [sim x t]
  (assert (<= (:tl sim) t (:tn sim)) (format "(<= %s %s %s)" (:tl sim) t (:tn sim)))
  (let [model  (:model sim)
        e      (- t (:tl sim))
        state' ((:ext-update-fn model) (:state sim) e x)]
    (assoc sim
           :state state'
           :tl    t
           :tn    (+ t ((:time-advance-fn model) state')))))
#_
(defn con-update [sim x t]
  (ext-update (int-update sim) x 0))

(defn tl [sim] (:tl sim))
(defn tn [sim] (:tn sim))
