(ns pettomato.devs.models.atomic-model
  (:require
   [clojure.set :refer [subset?]]
   [pettomato.devs.lib.number :refer [infinity]]))

(defn atomic-model
  "An atomic model for Parallel DEVS.

  Keyword options:

    initial-state - The initial state for the model. Can be anything; it is
  opaque to the simulator.

    initial-elapsed-time - The amount of time that the model has been in its
  initial state. Defaults to 0.

    internal-update - A function that takes a state and returns a new
  state. Called when the model is imminent.

    external-update - A function that takes a state, an elapsed time, and a bag
  of messages, and returns a new state.

    confluent-update - A function (or keyword, see below) that takes a state and
  a bag of messages, and returns a new state. Called when the model is imminent
  and has incoming messages. The bag of messages is implemented as a map from
  ports to sequences of values. For convenience, special keywords can be
  supplied instead of a function, and a corresponding function will be created:

      :internal-first - Generates a function that calls internal-update and then
  external-update. This is the default if confluent-update is not specified.

      :external-first - Generates a function that calls external-update and then
  internal-update.

    output - A function that takes a state and returns a bag of messages.

    time-advance - A function that takes a state and returns a non-negative
  number indicating the time until the model is imminent, provided it does not
  receive any messages before that time.

  Notes:

    A bag of messages is implemented as a map from ports to (unordered) sequences
  of messages."
  [& {:keys [initial-state
             initial-elapsed-time
             internal-update
             external-update
             confluent-update
             output
             time-advance]
      :or   {initial-elapsed-time 0
             internal-update      identity
             external-update      (fn [s e x] s)
             output               (constantly {})
             time-advance         (constantly infinity)}
      :as   options}]
  (let [confluent-update (cond
                           (ifn? confluent-update)                   confluent-update
                           (or (nil? confluent-update)
                               (= :internal-first confluent-update)) (fn [s x] (external-update (internal-update s) 0 x))
                           (= :external-first confluent-update)      (fn [s x] (internal-update (external-update s (time-advance s) x))))]
    (assert (number? initial-elapsed-time))
    (assert (ifn? internal-update))
    (assert (ifn? external-update))
    (assert (ifn? confluent-update))
    (assert (ifn? output))
    (assert (ifn? time-advance))
    (assert (empty? (dissoc options
                            :initial-state
                            :initial-elapsed-time
                            :internal-update
                            :external-update
                            :confluent-update
                            :output
                            :time-advance)))
    {:initial-total-state [initial-state initial-elapsed-time]
     :internal-update     internal-update
     :external-update     external-update
     :confluent-update    confluent-update
     :output              output
     :time-advance        time-advance}))

(defn atomic-model? [model]
  (and (map? model)
       (subset? #{:initial-total-state
                  :internal-update
                  :external-update
                  :confluent-update
                  :output
                  :time-advance}
                (set (keys model)))))

(defn ->rt
  "Wrap an atomic model to support real-time behavior.

  Real-time models must handle external-updates without messages. These null
  messages will be received even if the model is not connected to another model.
  Non-real-time models may be constructed under the simplifying assumptions that
  1. a model that isn't connected to any other model will never have its
  external-update function called, and 2. if the external-update function is
  called, it will be passed messages.

  This function wraps a non-real-time model with function implementations that
  will preserve the logic of the non-real-time model, but support the
  requirements of a real-time model."
  [model]
  (let [{:keys [initial-total-state
                internal-update
                external-update
                confluent-update
                output
                time-advance]} model
        [state elapsed]        initial-total-state]
    (atomic-model
     :initial-state        {:state   state
                            :elapsed 0}
     :initial-elapsed-time elapsed
     :internal-update      (fn [state']
                             (-> state'
                                 (update :state internal-update)
                                 (assoc :elapsed 0)))
     :external-update      (fn [state' elapsed-time messages]
                             (let [elapsed (+ (:elapsed state') elapsed-time)]
                               (if (seq messages)
                                 (-> state'
                                     (update :state external-update elapsed messages)
                                     (assoc :elapsed 0))
                                 (-> state'
                                     (assoc :elapsed elapsed)))))
     :confluent-update     (fn [state' messages]
                             (if (seq messages)
                               (-> state'
                                   (update :state confluent-update messages)
                                   (assoc :elapsed 0))
                               (-> state'
                                   (update :state internal-update)
                                   (assoc :elapsed 0))))
     :output               (comp output :state)
     :time-advance         (fn [state']
                             (let [{:keys [state elapsed]} state']
                               (- (time-advance state) elapsed))))))
