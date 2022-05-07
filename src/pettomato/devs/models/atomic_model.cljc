(ns pettomato.devs.models.atomic-model
  #?(:cljs (:require-macros pettomato.devs.models.atomic-model))
  (:require
   [pettomato.devs.lib.hyperreal :as h]))

(defprotocol AtomicModel
  "A protocol for atomic models.

  Implementations must adhere to these constraints:

  1. (time-advance (external-update s e {})) = (- (time-advance s) e)
  2. (confluent-update s {}) = (internal-update s)"
  (internal-update [state]
    "The internal state-transition function. Takes a state and returns a
  new state. Invoked when the model is imminent.")
  (external-update [state elapsed mail]
    "The external state-transition function. Takes a state, an elapsed
  time, and a bag of messages, and returns a new state. Invoked when
  the model has incoming messages.")
  (confluent-update [state mail]
    "The confluent state-transition function. Takes a state and a bag of
messages, and returns a new state. Invoked when the model is both
imminent and has incoming messages.")
  (output [state]
    "The output function. How the model emits messages. Takes a state and
returns a bag of messages. Invoked when the model is imminent, just
before the internal state-transition function is invoked.")
  (time-advance [state]
    "Takes a state and returns a non-zero positive hyperreal number
indicating the time until the model is imminent, provided it does not
receive any messages before that time."))

(defmacro def-atomic-model
  "A convenience macro that creates a record that implements AtomicModel
  from a possibly incomplete specification. Missing methods are given
  default implementations with the following return values:

     internal-update: state
     external-update: state
    confluent-update: (external-update (internal-update state) 0 mail)
              output: {}
        time-advance: infinity"
  [name [& fields] & specs]
  (let [syms  (set (map first specs))
        specs (cond-> specs
                (not (contains? syms 'internal-update)) (conj `(internal-update [state#] state#))
                (not (contains? syms 'external-update)) (conj `(external-update [state# elapsed# mail#] state#))
                (not (contains? syms 'output))          (conj `(output [state#] {}))
                ;; TODO: How to ensure h is loaded?
                (not (contains? syms 'time-advance))    (conj `(time-advance [state#] h/infinity)))
        ;; confluent-update update depends on internal-update and external-update.
        specs (cond-> specs
                (not (contains? syms 'confluent-update)) (conj `(confluent-update [state# mail#]
                                                                  (-> (internal-update state#)
                                                                      (external-update h/zero mail#)))))]
   `(defrecord ~name [~@fields]
      AtomicModel
      ~@specs)))

(defn atomic-model? [x] (satisfies? AtomicModel x))
