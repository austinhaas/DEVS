(ns pettomato.devs.examples.models
  (:require
   [pettomato.devs.models.atomic-model :refer [def-atomic-model internal-update external-update time-advance]]
   [pettomato.devs.models.executive-model :refer [def-executive-model structure-changes]]
   [pettomato.devs.models.network-model :refer [network-model]]
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
  (let [xs (lazy-seq (cons (first xs)
                           ;; Compensate for transitions taking 1ε,
                           ;; for the NIA.
                           (map (fn [[delay output]]
                                  [(h/- delay h/epsilon)
                                   output])
                                (rest xs))))]
    (->Generator xs)))

(def-atomic-model Buffer [delta buffer sigma]
  (internal-update [state] (assoc state :buffer nil :sigma h/infinity))
  (external-update [state elapsed mail]
    (if buffer
      (update state :sigma h/- elapsed)
      (assoc state
             :buffer (rand-nth (:in mail))
             :sigma  (h/- delta h/epsilon))))
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
             :sigma  (h/- delta h/epsilon))))
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
                (let [t (h/+ total-elapsed (h/- delta h/epsilon))]
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

(def-executive-model SimpleExec [structure-changes]
  (internal-update [state] (update state :structure-changes empty))
  (external-update [state elapsed mail] (update state :structure-changes into (:in mail)))
  (time-advance [state] (if (seq structure-changes) h/zero h/infinity))
  (structure-changes [state] structure-changes))

(defn simple-executive
  "A network executive that translates a fixed set of incoming messages
  into structure change messages."
  []
  (->SimpleExec []))

(def-executive-model StaticExec [])

(defn static-executive []
  (->StaticExec))

(defn simple-network-model
  ([executive-id]
   (simple-network-model executive-id [] []))
  ([executive-id models routes]
   (network-model executive-id [(simple-executive) h/zero] models routes)))

(defn static-network-model
  [models routes]
  (network-model (gensym) [(static-executive) h/zero] models routes))
