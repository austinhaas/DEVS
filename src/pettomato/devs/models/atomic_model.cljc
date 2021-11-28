(ns pettomato.devs.models.atomic-model
  (:require
   [clojure.set :refer [subset?]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :refer [merge-local-mail]]))

(defn atomic-model
  "Creates an atomic model.

  Optional keyword args:

    initial-state - The initial state for the model. Can be anything; it is
  opaque to the simulator and only used by the state transition functions
  supplied to this constructor. Default: nil.

    initial-elapsed-time - The amount of time that the model has been in its
  initial state. Default: zero ∈ hyperreals

    internal-update - The internal state transition function. A function that
  takes a state and returns a new state. Invoked when the model is
  imminent. Default: `identity`.

    external-update - The external state transition function. A function that
  takes a state, an elapsed time, and a bag of messages, and returns a new state
  and a bag of outgoing messages. Invoked when the model has incoming
  messages. Default: A \"no-op\" function that returns state.

    confluent-update - The confluent state transition function. A function (or
  keyword, see below) that takes a state and a bag of messages, and returns a
  new state. Invoked when the model is imminent and has incoming messages.
  Default: :internal-first.

      For convenience, instead of a function, special keywords can be supplied,
  which indicate a function to automatically generate:

      :internal-first - Generates a function that invokes internal-update and
  then external-update. This is the default if confluent-update is not
  specified. Output messages are combined via (merge-with into
  interal-update-mail external-update-mail).

      :external-first - Generates a function that invokes external-update and
  then internal-update. Output messages are combined via (merge-with into
  exteral-update-mail internal-update-mail).

    output - How the model emits messages. A function that takes a
  state and returns a bag of messages. Default: (constantly {}).

    petitions - How the model emits petitions. A function that takes a
  state and returns a collection of petitions. Default: (constantly []).

    time-advance - A function that takes a state and returns a non-zero positive
  hyperreal number indicating the time until the model is imminent, provided it
  does not receive any messages before that time. Default: (constantly infinity).

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
             petitions
             time-advance]
      :or   {initial-state        nil
             initial-elapsed-time h/zero
             internal-update      (fn no-op-int-update [state] state)
             external-update      (fn no-op-ext-update [state elapsed-time mail] state)
             confluent-update     :internal-first
             output               (constantly {})
             petitions            (constantly {})
             time-advance         (constantly h/infinity)}
      :as   options}]
  (let [confluent-update (case confluent-update
                           :internal-first (fn [state mail]
                                             (-> (internal-update state)
                                                 (external-update h/zero mail)))
                           :external-first (fn [state mail]
                                             (-> (external-update state (time-advance state) mail)
                                                 internal-update))
                           confluent-update)]
    (when-not (h/hyperreal? initial-elapsed-time)
      (throw (ex-info (str ":initial-elapsed-time must be a number; value: " initial-elapsed-time)
                      {})))
    (when-not (ifn? internal-update)
      (throw (ex-info (str ":internal-update must implement IFn; value: " internal-update)
                      {})))
    (when-not (ifn? external-update)
      (throw (ex-info (str ":external-update must implement IFn; value: " external-update)
                      {})))
    (when-not (ifn? confluent-update)
      (throw (ex-info (str ":confluent-update must implement IFn; value: " confluent-update)
                      {})))
    (when-not (ifn? output)
      (throw (ex-info (str ":output must implement IFn; value: " output)
                      {})))
    (when-not (ifn? petitions)
      (throw (ex-info (str ":petitions must implement IFn; value: " petitions)
                      {})))
    (when-not (ifn? time-advance)
      (throw (ex-info (str ":time-advance must implement IFn; value: " time-advance)
                      {})))
    (let [extra-options (dissoc options
                                :initial-state
                                :initial-elapsed-time
                                :internal-update
                                :external-update
                                :confluent-update
                                :output
                                :petitions
                                :time-advance)]
      (when (seq extra-options)
        (throw (ex-info (str "Invalid options supplied to atomic-model: " extra-options)
                        {}))))
    ;; Consider storing the original model definition, for later reference.
    {:initial-total-state [initial-state initial-elapsed-time]
     :internal-update     internal-update
     :external-update     external-update
     :confluent-update    confluent-update
     :output              output
     :petitions           petitions
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
                  :petitions
                  :time-advance}
                (set (keys model)))))
