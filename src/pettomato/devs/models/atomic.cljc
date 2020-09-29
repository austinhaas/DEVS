(ns pettomato.devs.models.atomic
  (:require
   [clojure.set :refer [subset?]]))

(defn atomic-model
  "An atomic Parallel DEVS model.

  initial-total-state - [initial-state elapsed-time]

  internal-update-fn - A function that takes a state and returns a new
  state. Called when the model is imminent.

  external-update-fn - A function that takes a state, an elapsed time, and a bag
  of messages, and returns a new state. Called when the model has incoming
  messages, but is not imminent.

  confluent-update-fn - A function that takes a state and a bag of messages, and
  returns a new state. Called when the model is imminent and has incoming
  messages.

  output-fn - A function that takes a state and returns a bag of messages.

  time-advance-fn - A function that takes a state and returns a non-negative
  number indicating the time until the model is imminent, provided it does not
  receive any messages before that time.

  A bag of messages is a map from ports to (unordered) sequences of messages."
  [initial-total-state
   internal-update-fn
   external-update-fn
   confluent-update-fn
   output-fn
   time-advance-fn]
  (assert (and (sequential? initial-total-state)
               (= 2 (count initial-total-state))
               (number? (second initial-total-state))))
  (assert (or (nil? internal-update-fn) (ifn? internal-update-fn)))
  (assert (or (nil? external-update-fn) (ifn? external-update-fn)))
  (assert (or (nil? confluent-update-fn) (ifn? confluent-update-fn)))
  (assert (or (nil? output-fn) (ifn? output-fn)))
  (assert (or (nil? time-advance-fn)  (ifn? time-advance-fn)))
  {:initial-total-state initial-total-state
   :internal-update-fn  internal-update-fn
   :external-update-fn  external-update-fn
   :confluent-update-fn (or confluent-update-fn
                            (fn [s x] (external-update-fn (internal-update-fn s) 0 x)))
   :output-fn           output-fn
   :time-advance-fn     time-advance-fn})

(defn atomic-model? [model]
  (and (map? model)
       (subset? #{:initial-total-state
                  :internal-update-fn
                  :external-update-fn
                  :confluent-update-fn
                  :output-fn
                  :time-advance-fn}
                (set (keys model)))))
