(ns pettomato.devs.models.network-executive-model
  #?(:cljs (:require-macros pettomato.devs.models.network-executive-model))
  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.models.atomic-model :refer [AtomicModel]]))

(defprotocol NetworkExecutiveModel
  "A protocol for network executive models."
  (structure-changes [state]
    "Returns a seq of network structure changes."))

(defmacro def-network-executive-model
  "A convenience macro that creates a record that implements AtomicModel
  and NetworkExecutiveModel from a possibly incomplete
  specification. Missing methods are given default implementations
  with the following return values:

     internal-update: state
     external-update: state
    confluent-update: (external-update (internal-update state) 0 mail)
              output: {}
        time-advance: infinity
   network-structure: []"
  [name [& fields] & specs]
  (let [all-atomic-syms       #{'internal-update 'external-update 'confluent-update 'output 'time-advance}
        all-exec-syms         #{'structure-changes}
        provided-atomic-specs (filter (comp all-atomic-syms first) specs)
        provided-exec-specs   (filter (comp all-exec-syms first) specs)
        provided-atomic-syms  (set (map first provided-atomic-specs))
        provided-exec-syms    (set (map first provided-exec-specs))
        atomic-specs          (cond-> provided-atomic-specs
                                (not (contains? provided-atomic-syms 'internal-update))  (conj '(internal-update [state] state))
                                (not (contains? provided-atomic-syms 'external-update))  (conj '(external-update [state elapsed mail] state))
                                (not (contains? provided-atomic-syms 'output))           (conj '(output [state] {}))
                                (not (contains? provided-atomic-syms 'time-advance))     (conj '(time-advance [state] h/infinity))) ; TODO: How to ensure h is loaded?
        ;; confluent-update update depends on internal-update and external-update.
        atomic-specs          (cond-> atomic-specs
                                (not (contains? provided-atomic-syms 'confluent-update)) (conj '(confluent-update [state mail]
                                                                                                  (-> (internal-update state)
                                                                                                      (external-update h/zero mail)))))
        exec-specs            (cond-> provided-exec-specs
                                (not (contains? provided-exec-syms 'structure-changes)) (conj '(structure-changes [state] [])))]
   `(defrecord ~name [~@fields]
      AtomicModel
      ~@atomic-specs
      NetworkExecutiveModel
      ~@exec-specs)))

(defn executive-model? [x] (satisfies? NetworkExecutiveModel x))
