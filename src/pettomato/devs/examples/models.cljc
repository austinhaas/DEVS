(ns pettomato.devs.examples.models
  (:require
   [pettomato.devs :refer [infinity atomic-model trace]]))

(defn generator
  "A model that periodically emits value on a port labeled :out."
  [period value]
  (atomic-model
   (let [s nil
         e 0]
     [s e])
   identity
   nil
   nil
   (constantly {:out [value]})
   (constantly period)))

(defn lazy-seq-generator
  "A model that emits values according to a (possibly lazy and infinite) seq
  of [sigma mail]."
  [s]
  (atomic-model
   (let [s s
         e 0]
     [s e])
   next
   nil
   nil
   (comp second first)
   (fn time-advance [s]
     (if (seq s)
       (ffirst s)
       infinity))))

(defn delay1
  "A model that receives messages on port :in and emits the same message on
  port :out after processing-time. Can queue multiple messages simultaneously."
  [processing-time]
  (atomic-model
   (let [s {:queue (sorted-map)
            :delta 0}
         e 0]
     [s e])
   (fn internal-update  [state]
     (-> state
         (update :queue dissoc (ffirst (:queue state)))
         (assoc :delta (ffirst (:queue state)))))
   (fn external-update  [state elapsed-time messages]
     (let [delta (+ (:delta state) elapsed-time)
           t     (+ delta processing-time)]
       (-> state
           (update-in [:queue t] into (:in messages))
           (assoc :delta delta))))
   nil
   (fn output           [state]
     {:out (second (first (:queue state)))})
   (fn time-advance     [state]
     (if (empty? (:queue state))
       infinity
       (- (ffirst (:queue state))
          (:delta state))))))

(defn delay2
  "Like delay1, but processing-time is set by each message, with the
  format [processing-time value]."
  []
  (atomic-model
   (let [s {:queue (sorted-map)
            :delta 0}
         e 0]
     [s e])
   (fn internal-update  [state]
     (-> state
         (update :queue dissoc (ffirst (:queue state)))
         (assoc :delta (ffirst (:queue state)))))
   (fn external-update  [state elapsed-time messages]
     (trace "external-update: %s" messages)
     (let [delta (+ (:delta state) elapsed-time)]
       (reduce (fn [state [processing-time value]]
                 (let [t (+ delta processing-time)]
                   (update-in state [:queue t] conj value)))
               (assoc state :delta delta)
               (:in messages))))
   nil
   (fn output           [state]
     {:out (second (first (:queue state)))})
   (fn time-advance     [state]
     (if (empty? (:queue state))
       infinity
       (- (ffirst (:queue state))
          (:delta state))))))
