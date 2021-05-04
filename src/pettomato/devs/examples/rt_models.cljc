(ns pettomato.devs.examples.rt-models
  (:require
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]))

(defn rt-generator
  "*** For testing only ***.

  A model that periodically emits value on a port labeled :out.

  This implementation is intended to exercise the real-time system by simulating
  a real-time component. This is achieved by reporting a time-advance of
  infinity, until, via no-op external-updates, it is determined that the model
  is imminent, and then an appropriate time-advance value is returned.

  This should be used with rt-afap-root-coordinator. This DOES NOT account for
  scale-factor!

  step-size - Should be the same as the value provided to
  rt-afap-root-coordinator."
  [period value step-size]
  (atomic-model
   :initial-state   {:local-sim-time        0
                     :target-local-sim-time period}
   :internal-update (fn [state]
                      (let [t (:target-local-sim-time state)]
                        (assoc state
                               :local-sim-time        t
                               :target-local-sim-time (+ t period))))
   :external-update (fn [state elapsed-time mail]
                      (update state :local-sim-time + elapsed-time))
   :output          (constantly {:out [value]})
   :time-advance    (fn [state]
                      (let [t1 (:local-sim-time state)
                            t2 (:target-local-sim-time state)
                            t3 (+ t1 step-size)]
                        (if (<= t1 t2 t3)
                          (- t2 t1)
                          infinity)))))

(defn rt-lazy-seq-generator
  "*** For testing only ***.

  A model that emits values according to a (possibly lazy and infinite) seq
  of [sigma mail].

  s - A seq of [sigma mail] pairs.

  sigma - A number. The delay, in milliseconds, before the associated mail
  should be output.

  mail - A map from output ports to sequences of messages.

  Be careful not to print models containing lazy seqs!

  Example:
    ;; Outputs 3 messages on port :out, with a 100ms delay before each one.
    (rt-lazy-seq-generator [[100 {:out ['first]}]
                           [100 {:out ['second]}]
                           [100 {:out ['third]}]])

    ;; Outputs an infinite number of messages on port :out, with a random amount
    ;; of time before each.
    (rt-lazy-seq-generator (for [i (range)]
                             [(rand-int 1000) {:out [(str \"msg-\" i)]}]))

  This implementation is intended to exercise the real-time system by simulating
  a real-time component. This is achieved by reporting a time-advance of
  infinity, until, via no-op external-updates, it is determined that the model
  is imminent, and then an appropriate time-advance value is returned.

  This should be used with rt-afap-root-coordinator. This DOES NOT account for
  scale-factor!

  step-size - Should be the same as the value provided to
  rt-afap-root-coordinator"
 [s step-size]
  (atomic-model
   :initial-state   {:seq                   s
                     :local-sim-time        0
                     :target-local-sim-time (if (seq s)
                                              (ffirst s)
                                              infinity)}
   :internal-update (fn [state]
                      (let [t (:target-local-sim-time state)
                            s (next (:seq state))]
                        (assoc state
                               :local-sim-time        t
                               :target-local-sim-time (if (seq s)
                                                        (+ t (ffirst s))
                                                        infinity)
                               :seq                   s)))
   :external-update (fn [state elapsed-time mail]
                      (update state :local-sim-time + elapsed-time))
   :output          (comp second first :seq)
   :time-advance    (fn [state]
                      (let [t1 (:local-sim-time state)
                            t2 (:target-local-sim-time state)
                            t3 (+ t1 step-size)]
                        (if (<= t1 t2 t3)
                          (- t2 t1)
                          infinity)))))

(defn rt-single-delay
  "*** For testing only ***.

  A model that receives messages on port :in and sends them back out on port :out
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

  An atomic model.

  This implementation is intended to exercise the real-time system by simulating
  a real-time component. This is achieved by reporting a time-advance of
  infinity, until, via no-op external-updates, it is determined that the model
  is imminent, and then an appropriate time-advance value is returned.

  This should be used with rt-afap-root-coordinator. This DOES NOT account for
  scale-factor!

  step-size - Should be the same as the value provided to
  rt-afap-root-coordinator"

  [duration step-size & {:keys [priority]
                         :or {priority :internal-first}}]
  (atomic-model
   :initial-state    {:phase                 :passive
                      :value                 nil
                      :output                nil
                      :local-sim-time        0
                      :target-local-sim-time infinity}
   :internal-update  (fn [state]
                       (let [t (:target-local-sim-time state)]
                         (case (:phase state)
                           :intake (assoc state
                                          :phase                 :active
                                          :local-sim-time        t
                                          :target-local-sim-time (+ t duration))
                           :active (assoc state
                                          :phase                 :fire
                                          :value                 nil
                                          :output                (:value state)
                                          :local-sim-time        t
                                          :target-local-sim-time t)
                           :fire   (if (:value state)
                                     (assoc state
                                            :phase                 :active
                                            :local-sim-time        t
                                            :target-local-sim-time (+ t duration))
                                     (assoc state
                                            :phase                 :passive
                                            :value                 nil
                                            :output                nil
                                            :local-sim-time        t
                                            :target-local-sim-time infinity)))))
   :external-update  (fn [state elapsed-time messages]
                       (assert (<= (count messages) 1)
                               "A single-delay model received more than one message at the same time. The behavior in that case is undefined, since those messages are unordered.")
                       (let [t (+ (:local-sim-time state)
                                  elapsed-time)]
                         (if (seq messages)
                           (case (:phase state)
                             :fire (assoc state
                                          :local-sim-time t
                                          :value          (first (:in messages)))
                             (assoc state
                                    :phase                 :intake
                                    :value                 (first (:in messages))
                                    :local-sim-time        t
                                    :target-local-sim-time t))
                           (assoc state :local-sim-time t))))
   :confluent-update priority
   :output           (fn [state]
                       (case (:phase state)
                         :fire {:out [(:output state)]}
                         nil))
   :time-advance     (fn [state]
                       (let [t1 (:local-sim-time state)
                             t2 (:target-local-sim-time state)
                             t3 (+ t1 step-size)]
                         (if (<= t1 t2 t3)
                           (- t2 t1)
                           infinity)))))
