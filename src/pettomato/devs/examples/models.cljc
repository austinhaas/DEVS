(ns pettomato.devs.examples.models
  (:require
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.lib.hyperreal :as h]))

(defn generator
  "A model that periodically emits value on a port labeled :out."
  [period value]
  (assert (and h/hyperreal? period) (h/pos? period))
  (atomic-model
   :output       (constantly {:out [value]})
   :time-advance (constantly period)))

;; What should happen if initial-elapsed time is greater than the sum of the
;; first dozen values in this seq?
(defn lazy-seq-generator
  "A model that emits values according to a (possibly lazy and infinite) seq
  of [sigma mail].

  coll - A collection of [sigma mail] pairs.

  sigma - A number. The delay, in milliseconds, before the associated mail
  should be output.

  mail - A map from output ports to sequences of messages.

  Be careful not to print models containing lazy seqs!

  Example:
    ;; Outputs 3 messages on port :out, with a 100ms delay before each one.
    (lazy-seq-generator [[100 {:out ['first]}]
                         [100 {:out ['second]}]
                         [100 {:out ['third]}]])

    ;; Outputs an infinite number of messages on port :out, with a random amount
    ;; of time before each.
    (lazy-seq-generator (for [i (range)]
                          [(rand-int 1000) {:out [(str \"msg-\" i)]}]))"
  ([coll]
   (lazy-seq-generator coll h/zero))
  ([coll initial-elapsed-time]
   (atomic-model
    :initial-state        coll
    :initial-elapsed-time initial-elapsed-time
    :internal-update      next
    :output               (comp second first)
    :time-advance         (fn [coll]
                            (if (seq coll)
                              (ffirst coll)
                              h/infinity)))))

(defn lazy-seq-petition-generator
  "A model that emits values according to a (possibly lazy and infinite) seq
  of [sigma mail].

  coll - A collection of [sigma mail] pairs.

  sigma - A number. The delay, in milliseconds, before the associated mail
  should be output.

  mail - A map from output ports to sequences of messages.

  Be careful not to print models containing lazy seqs!

  Example:
    ;; Outputs 3 messages on port :out, with a 100ms delay before each one.
    (lazy-seq-generator [[100 {:out ['first]}]
                         [100 {:out ['second]}]
                         [100 {:out ['third]}]])

    ;; Outputs an infinite number of messages on port :out, with a random amount
    ;; of time before each.
    (lazy-seq-generator (for [i (range)]
                          [(rand-int 1000) {:out [(str \"msg-\" i)]}]))"
  ([coll]
   (lazy-seq-petition-generator coll h/zero))
  ([coll initial-elapsed-time]
   (atomic-model
    :initial-state        coll
    :initial-elapsed-time initial-elapsed-time
    :internal-update      next
    :petitions            (comp second first)
    :time-advance         (fn [coll]
                            (if (seq coll)
                              (ffirst coll)
                              h/infinity)))))

(defn single-delay
  "A model that receives messages on port :in and sends them back out on port :out
  after duration. Only one message can be delayed at a time. If a new message is
  received before a previously received message has been sent, it will be
  discarded.

  Args:

    duration - A number. The amount of time to wait, in milliseconds, between
  receiving a message and resending it.

  Optional keyword args:

    priority - A keyword: :internal-first or :external-first. Determines the
  behavior when the model receives a new message at exactly the same time that
  it is scheduled to send a delayed message. If :internal-first, then the delay
  is ready to receive a new input. If :external-first, then the delay is not
  ready to receive a new input. In other words, :internal-first means the delay
  is busy up until the time it sends output, and :external-first means the delay
  is busy through the time is sends output. This was added to demonstrate how
  confluence works.

  Returns:

  An atomic model."
  [duration & {:keys [priority]
               :or {priority :internal-first}}]
  (assert (and h/hyperreal? duration) (h/pos? duration))
  (atomic-model
   :initial-state    {:value nil :sigma h/infinity}
   :internal-update  (fn [state]
                       (assoc state :value nil :sigma h/infinity))
   :external-update  (fn [state elapsed-time messages]
                       (assert (not (next messages)) "single-delay doesn't know how to handle multiple simultaneous inputs")
                       (if (nil? (:value state))
                         (assoc state :value (first (:in messages)) :sigma duration)
                         (update state :sigma h/- elapsed-time)))
   :confluent-update priority
   :output           (fn [state] {:out [(:value state)]})
   :time-advance     :sigma))

(defn fixed-delay
  "A model that receives messages on port :in and sends them back out on port :out
  after duration. Multiple messages may be delayed simultaneously."
  [duration]
  (assert (and h/hyperreal? duration) (h/pos? duration))
  (atomic-model
   :initial-state   {:queue (sorted-map-by h/comparator)
                     :delta h/zero}
   :internal-update (fn [state]
                      (-> state
                          (update :queue dissoc (ffirst (:queue state)))
                          (assoc :delta (ffirst (:queue state)))))
   :external-update (fn [state elapsed-time messages]
                      (let [delta (h/+ (:delta state) elapsed-time)
                            t     (h/+ delta duration)]
                        (-> state
                            (update-in [:queue t] into (:in messages))
                            (assoc :delta delta))))
   :confluent-update :internal-first
   :output          (fn [state]
                      {:out (second (first (:queue state)))})
   :time-advance    (fn [state]
                      (if (empty? (:queue state))
                        h/infinity
                        (h/- (ffirst (:queue state))
                             (:delta state))))))

(defn variable-delay
  "A model that receives messages of the form [duration value] on port :in,
  and sends value back out on port :out after duration. Multiple messages may be
  delayed simultaneously."
  []
  (atomic-model
   :initial-state   {:queue (sorted-map-by h/comparator)
                     :delta h/zero}
   :internal-update (fn [state]
                      (-> state
                          (update :queue dissoc (ffirst (:queue state)))
                          (assoc :delta (ffirst (:queue state)))))
   :external-update (fn [state elapsed-time messages]
                      (let [delta (h/+ (:delta state) elapsed-time)]
                        (reduce (fn [state [duration value]]
                                  (assert (and h/hyperreal? duration) (h/pos? duration))
                                  (let [t (h/+ delta duration)]
                                    (update-in state [:queue t] conj value)))
                                (assoc state :delta delta)
                                (:in messages))))
   :output          (fn [state]
                      {:out (second (first (:queue state)))})
   :time-advance    (fn [state]
                      (if (empty? (:queue state))
                        h/infinity
                        (h/- (ffirst (:queue state))
                             (:delta state))))))
