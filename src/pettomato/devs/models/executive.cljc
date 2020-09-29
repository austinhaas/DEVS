(ns pettomato.devs.models.executive
  (:require
   [clojure.set :refer [subset?]]
   [pettomato.devs.models.atomic :refer [atomic-model]]))

(defn executive-model
  "An executive model, loosely based on DSDE.

  Barros. Abstract Simulators for the DSDE Formalism. 1998.
  https://repository.lib.ncsu.edu/bitstream/handle/1840.4/6989/1998_0056.pdf

  This is a subclass of an atomic model.

  network-fn - A function that takes a state and returns a network structure."
  [initial-total-state
   internal-update-fn
   external-update-fn
   confluent-update-fn
   output-fn
   time-advance-fn
   network-fn]
  (-> (atomic-model initial-total-state
                    internal-update-fn
                    external-update-fn
                    confluent-update-fn
                    output-fn
                    time-advance-fn)
      (assoc :network-fn network-fn)))

(defn executive-model? [model]
  (and (map? model)
       (subset? #{:initial-total-state
                  :internal-update-fn
                  :external-update-fn
                  :confluent-update-fn
                  :output-fn
                  :time-advance-fn
                  :network-fn}
                (set (keys model)))))
