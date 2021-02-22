(ns pettomato.devs.examples.models
  (:require
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.lib.number :refer [infinity]]))

(defn generator
  "A model that periodically emits value on a port labeled :out."
  [period value]
  (atomic-model
   :output       (constantly {:out [value]})
   :time-advance (constantly period)))

(defn lazy-seq-generator
  "A model that emits values according to a (possibly lazy and infinite) seq
  of [sigma mail]."
  [s]
  (atomic-model
   :initial-state   s
   :internal-update next
   :output          (comp second first)
   :time-advance    (fn [s]
                      (if (seq s)
                        (ffirst s)
                        infinity))))

(defn delay1
  "A model that receives messages on port :in and emits the same message on
  port :out after processing-time. Can queue multiple messages simultaneously."
  [processing-time]
  (atomic-model
   :initial-state   {:queue (sorted-map)
                     :delta 0}
   :internal-update (fn [state]
                      (-> state
                          (update :queue dissoc (ffirst (:queue state)))
                          (assoc :delta (ffirst (:queue state)))))
   :external-update (fn [state elapsed-time messages]
                      (let [delta (+ (:delta state) elapsed-time)
                            t     (+ delta processing-time)]
                        (-> state
                            (update-in [:queue t] into (:in messages))
                            (assoc :delta delta))))
   :output          (fn [state]
                      {:out (second (first (:queue state)))})
   :time-advance    (fn [state]
                      (if (empty? (:queue state))
                        infinity
                        (- (ffirst (:queue state))
                           (:delta state))))))

(defn delay2
  "Like delay1, but processing-time is set by each message, with the
  format [processing-time value]."
  []
  (atomic-model
   :initial-state   {:queue (sorted-map)
                     :delta 0}
   :internal-update (fn [state]
                      (-> state
                          (update :queue dissoc (ffirst (:queue state)))
                          (assoc :delta (ffirst (:queue state)))))
   :external-update (fn [state elapsed-time messages]
                      (let [delta (+ (:delta state) elapsed-time)]
                        (reduce (fn [state [processing-time value]]
                                  (let [t (+ delta processing-time)]
                                    (update-in state [:queue t] conj value)))
                                (assoc state :delta delta)
                                (:in messages))))
   :output          (fn [state]
                      {:out (second (first (:queue state)))})
   :time-advance    (fn [state]
                      (if (empty? (:queue state))
                        infinity
                        (- (ffirst (:queue state))
                           (:delta state))))))
