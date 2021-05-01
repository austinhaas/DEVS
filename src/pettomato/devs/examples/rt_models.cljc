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
  [duration priority step-size]
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
