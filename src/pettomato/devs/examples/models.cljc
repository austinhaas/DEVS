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
  [coll]
  (atomic-model
   :initial-state   coll
   :internal-update next
   :output          (comp second first)
   :time-advance    (fn [coll]
                      (if (seq coll)
                        (ffirst coll)
                        infinity))))

(defn single-delay
  "A model that receives messages on port :in and sends them back out on port :out
  after duration. Only one message can be delayed at a time. If a new message is
  received before a previously received message has been sent, the previously
  received message will be discarded.

  Args:

    duration - A number. The amount of time to wait, in milliseconds, between
  receiving a message and resending it.

  Optional keyword args:

    priority - A keyword: :internal-first or :external-first. Determines the
  behavior when the model receives a new message at exactly the same time that
  it is scheduled to send a delayed message. If :internal-first, then the older
  message will be sent before the new message is processed. If :external-first,
  then the older message will be discarded when the new message is
  received. Defaults to :internal-first.

  Returns:

  An atomic model."
  [duration & {:keys [priority]
               :or {priority :internal-first}}]
  ;; This may not be a good example for how to code this behavior. This was
  ;; originally created to test confluence. I wanted something with obvious
  ;; behavior that would differ solely based on which state transition function
  ;; was prioritized. In reality, you probably won't be designing models that
  ;; expose the confluent function this way, and if you are doing anything
  ;; interesting with the confluent function, you would probably just code a
  ;; custom one and avoid the challenge of designing the other state transitions
  ;; so that they'd work in either order.

  ;; `pettomato.devs-test/confluence-tests` depends on this implementation.
  (atomic-model
   :initial-state    {:phase  :passive
                      :value  nil
                      :output nil
                      :sigma  infinity}
   :internal-update  (fn [state]
                       (case (:phase state)
                         :intake (assoc state
                                        :phase :active
                                        :sigma duration)
                         :active (assoc state
                                        :phase  :fire
                                        :value  nil
                                        :output (:value state)
                                        :sigma  0)
                         :fire   (if (:value state)
                                   (assoc state
                                          :phase :active
                                          :sigma duration)
                                   (assoc state
                                          :phase  :passive
                                          :value  nil
                                          :output nil
                                          :sigma  infinity))))
   :external-update  (fn [state elapsed-time messages]
                       (assert (= 1 (count messages))
                               "A single-delay model received more than one message at the same time. The behavior in that case is undefined, since those messages are unordered.")
                       (case (:phase state)
                         :fire (assoc state
                                      :value (first (:in messages)))
                         (assoc state
                                :phase :intake
                                :value (first (:in messages))
                                :sigma 0)))
   :confluent-update priority
   :output           (fn [state]
                       (case (:phase state)
                         :fire {:out [(:output state)]}
                         nil))
   :time-advance     :sigma))

(defn fixed-delay
  "A model that receives messages on port :in and sends them back out on port :out
  after duration. Multiple messages may be delayed simultaneously."
  [duration]
  (atomic-model
   :initial-state   {:queue (sorted-map)
                     :delta 0}
   :internal-update (fn [state]
                      (-> state
                          (update :queue dissoc (ffirst (:queue state)))
                          (assoc :delta (ffirst (:queue state)))))
   :external-update (fn [state elapsed-time messages]
                      (let [delta (+ (:delta state) elapsed-time)
                            t     (+ delta duration)]
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

(defn variable-delay
  "A model that receives messages of the form [duration value] on port :in,
  and sends value back otu on port :out after duration. Multiple messages may be
  delayed simultaneously."
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
                        (reduce (fn [state [duration value]]
                                  (let [t (+ delta duration)]
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
