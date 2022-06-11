(ns pettomato.devs.examples
  "Some useful models."
  (:require
   [pettomato.devs :as devs]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.priority-queue :as pq]))

(devs/def-atomic-model Generator [xs]
  (internal-update [state] (update state :xs next))
  (output [state] (when (first xs) {:out (second (first xs))}))
  (time-advance [state] (or (ffirst xs) h/infinity)))

(defn generator
  "Creates a generator atomic model from a (possibly lazy) sequence
  of [delta output] pairs. The model emits each output on port :out
  after delta. Sleeps forever after emitting the last output."
  [xs]
  (->Generator xs))

(devs/def-atomic-model Buffer [delta buffer sigma]
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

(devs/def-atomic-model Buffer2 [delta buffer sigma]
  (internal-update [state] (assoc state :buffer nil :sigma h/infinity))
  (external-update [state elapsed mail]
    (if buffer
      (update state :sigma h/- elapsed)
      (assoc state
             :buffer (rand-nth (:in mail))
             :sigma  delta)))
  (confluent-update [state mail]
    (-> (devs/external-update state (devs/time-advance state) mail)
        (devs/internal-update)))
  (output [state] {:out [buffer]})
  (time-advance [state] sigma))

(defn buffer2
  "Like `buffer`, but the confluent function prioritizes external
  updates over internal updates, so if a new input arrives at the same
  time as the model is imminent, it will be discarded (because the
  model isn't done processing the previous input)."
  [delta]
  (->Buffer2 delta nil h/infinity))

(devs/def-atomic-model Buffer+ [total-elapsed queue]
  (internal-update [state]
    (-> state
        (update :total-elapsed h/+ (devs/time-advance state))
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
