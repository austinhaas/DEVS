(ns pettomato.devs.models.atomic-model
  (:require
   [clojure.set :refer [subset?]]
   [pettomato.devs.lib.number :refer [infinity]]))

(defn atomic-model
  "Creates an atomic model.

  Optional keyword args:

    initial-state - The initial state for the model. Can be anything; it is
  opaque to the simulator and only used by the state transition functions
  supplied to this constructor. Default: nil.

    initial-elapsed-time - The amount of time that the model has been in its
  initial state. Default: 0.

    internal-update - The internal state transition function. A function that
  takes a state and returns a new state. Invoked when the model is
  imminent. Default: identity.

    external-update - The external state transition function. A function that
  takes a state, an elapsed time, and a bag of messages, and returns a new
  state. Invoked when the model has incoming messages. Default: A \"no-op\"
  function that returns state.

    confluent-update - The confluent state transition function. A function (or
  keyword, see below) that takes a state and a bag of messages, and returns a
  new state. Invoked when the model is imminent and has incoming
  messages. Default: :internal-first.

      For convenience, instead of a function, special keywords can be supplied,
      which indicate a function to automatically generate:

      :internal-first - Generates a function that invokes internal-update and
      then external-update. This is the default if confluent-update is not
      specified.

      :external-first - Generates a function that invokes external-update and then
      internal-update.

    output - How the model emits messages. A function that takes a state and
  returns a bag of messages. Default: (constantly {}).

    time-advance - A function that takes a state and returns a non-negative
  number indicating the time until the model is imminent, provided it does not
  receive any messages before that time. Default: (constantly infinity).

  Returns:

    An atomic model.

  Notes:

    A \"bag of messages\" is implemented as a map from ports to (unordered)
  sequences of messages. This is also referred to as mail and local mail.

  Implementation notes:

    Technically, the \"bag of messages\" is an opaque data structure here. The
  actual structure depends on the network simulator, which handles message
  routing. For example, this implementation would equally apply to DEVS without
  ports, or for ordered sequences of messages."
  [& {:keys [initial-state
             initial-elapsed-time
             internal-update
             external-update
             confluent-update
             output
             time-advance]
      :or   {initial-state        nil
             initial-elapsed-time 0
             internal-update      identity
             external-update      (fn [state elapsed-time mail] state) ;; no-op
             confluent-update     :internal-first
             output               (constantly {})
             time-advance         (constantly infinity)}
      :as   options}]
  (let [confluent-update (cond
                           (= :internal-first confluent-update)
                           (fn [state mail]
                             (-> (internal-update state)
                                 (external-update 0 mail)))

                           (= :external-first confluent-update)
                           (fn [state mail]
                             (-> (external-update state (time-advance state) mail)
                                 internal-update))

                           (ifn? confluent-update)
                           confluent-update)]
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

(defn atomic-model?
  "Returns true if model is an atomic model, otherwise false."
  [model]
  (and (map? model)
       (subset? #{:initial-total-state
                  :internal-update
                  :external-update
                  :confluent-update
                  :output
                  :time-advance}
                (set (keys model)))))

(defn ->rt
  "Extend an atomic model to support real-time behavior.

  This function wraps a non-real-time model with function implementations that
  will preserve the logic of the non-real-time model, but support the
  requirements of a real-time model.

  Real-time models must handle external-updates without messages. These null
  messages will be received even if the model is not connected to another model.
  Non-real-time models may be constructed under the simplifying assumptions that
  1. a model that isn't connected to any other model will never have its
  external-update function invoked, and 2. if the external-update function is
  invoked, it will be passed messages.

  This function is intended to provide easy re-use of existing atomic models in
  a real-time context. Note that a model only needs to handle real-time behavior
  if its simulator is a real-time simulator. Non-real-time models may be
  included in a system that mixes real-time and non-real-time components;
  provided there is a root simulator for the non-real-time components that
  handles the real-time behavior, but does not propagate it to its components."
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
