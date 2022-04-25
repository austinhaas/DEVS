(ns pettomato.devs.examples.models
  (:require
   [pettomato.devs.models.atomic-model :refer [def-atomic-model internal-update external-update confluent-update output time-advance]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.priority-queue :as pq]))

(def-atomic-model Generator [xs]
  (internal-update [state] (update state :xs next))
  (output [state] (second (first xs)))
  (time-advance [state] (or (ffirst xs) h/infinity)))

(defn generator
  "Creates a generator atomic model from a (possibly lazy) sequence
  of [delta output] pairs. The model emits each output after
  delta. Sleeps forever after emitting the last output."
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

(defn buffer [delta]
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
  "Creates a delay atomic model, which receives messages in the
  form [delta value] on port :in, waits for delta, and then emits
  value on port :out. Multiple messages may be delayed
  simultaneously."
  []
  (map->Buffer+ {:total-elapsed h/zero
                 :queue         (pq/priority-queue h/comparator)}))
