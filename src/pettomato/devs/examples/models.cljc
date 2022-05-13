(ns pettomato.devs.examples.models
  "Some useful models."
  (:require
   [pettomato.devs.models.atomic-model :refer [def-atomic-model internal-update external-update time-advance]]
   [pettomato.devs.models.executive-model :refer [def-executive-model structure-changes]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.priority-queue :as pq]))

(def-atomic-model Generator [xs]
  (internal-update [state] (update state :xs next))
  (output [state] (when (first xs) {:out (second (first xs))}))
  (time-advance [state] (or (ffirst xs) h/infinity)))

(defn generator
  "Creates a generator atomic model from a (possibly lazy) sequence
  of [delta output] pairs. The model emits each output on port :out
  after delta. Sleeps forever after emitting the last output."
  [xs]
  (->Generator xs))

(def-atomic-model Buffer [delta buffer sigma]
  (internal-update [state] (assoc state :buffer nil :sigma h/infinity))
  (external-update [state elapsed mail]
    (if buffer
      (update state :sigma h/- elapsed)
      (assoc state
             :buffer (rand-nth (:in mail))
             :sigma  delta)))
  (output [state] {:out [buffer]})
  (time-advance [state] sigma))

(defn buffer
  "Creates a buffer atomic model. This model receives a value on
  port :in and emits the same value on port :out after delta time, a
  positive hyperreal. If multiple values are received at the same
  time, then one will be selected at random. If a new value is
  received before the currently buffered value is emitted, it will be
  ignored."
  [delta]
  (->Buffer delta nil h/infinity))

(def-atomic-model Buffer2 [delta buffer sigma]
  (internal-update [state] (assoc state :buffer nil :sigma h/infinity))
  (external-update [state elapsed mail]
    (if buffer
      (update state :sigma h/- elapsed)
      (assoc state
             :buffer (rand-nth (:in mail))
             :sigma  delta)))
  (confluent-update [state mail]
    (-> (external-update state (time-advance state) mail)
        (internal-update)))
  (output [state] {:out [buffer]})
  (time-advance [state] sigma))

(defn buffer2
  "Like `buffer`, but the confluent function prioritizes external
  updates over internal updates, so if a new input arrives at the same
  time as the model is imminent, it will be discarded (because the
  model isn't done processing the previous input)."
  [delta]
  (->Buffer2 delta nil h/infinity))

(def-atomic-model Buffer+ [total-elapsed queue]
  (internal-update [state]
    (-> state
        (update :total-elapsed h/+ (time-advance state))
        (update :queue pq/pop)))
  (external-update [state elapsed mail]
    (let [total-elapsed (h/+ total-elapsed elapsed)]
      (reduce (fn [state [delta value]]
                (assert (and (h/hyperreal? delta)
                             (h/pos? delta)))
                (let [t (h/+ total-elapsed delta)]
                  (update state :queue pq/insert t value)))
              (assoc state :total-elapsed total-elapsed)
              (:in mail))))
  (output [state] {:out (pq/peek queue)})
  (time-advance [state]
    (if (pq/empty? queue)
      h/infinity
      (h/- (pq/peek-key queue) total-elapsed))))

(defn buffer+
  "Creates an atomic buffer model capable of buffering many values
  simultaneously. The model accepts messages in the form [delta value]
  on port :in, waits for delta, and then emits value on port :out."
  []
  (map->Buffer+ {:total-elapsed h/zero
                 :queue         (pq/priority-queue h/comparator)}))

(def-executive-model SimpleExec [structure-changes]
  (internal-update [state] (update state :structure-changes empty))
  (external-update [state elapsed mail] (update state :structure-changes into (:in mail)))
  (time-advance [state] (if (seq structure-changes) h/epsilon h/infinity))
  (structure-changes [state] structure-changes))

(defn simple-executive
  "A network executive that accepts structure change messages on
  port :in and provides them as-is to the network simulator."
  []
  (->SimpleExec []))

(def-executive-model StaticExec [])

(defn static-executive []
  (->StaticExec))

(defn simple-network-model
  "A network model with a simple-executive."
  ([executive-id]
   (simple-network-model executive-id [] []))
  ([executive-id models routes]
   (network-model executive-id [(simple-executive) h/zero] models routes)))

(defn static-network-model
  "A network model for static networks. The model has a network
  executive, but it is inaccessible and does nothing."
  [models routes]
  (network-model (gensym) [(static-executive) h/zero] models routes))
