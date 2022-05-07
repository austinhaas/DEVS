(ns pettomato.devs.models.executive-model
  #?(:cljs (:require-macros pettomato.devs.models.executive-model))
  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.models.atomic-model :refer [AtomicModel
                                               internal-update external-update confluent-update
                                               output time-advance]]))

(defprotocol ExecutiveModel
  "A protocol for network executive models."
  (structure-changes [state]
    "Returns a seq of network structure changes."))

(defmacro def-executive-model
  "A convenience macro that creates a record that implements AtomicModel
  and ExecutiveModel from a possibly incomplete specification. Missing
  methods are given default implementations with the following return
  values:

     internal-update: state
     external-update: state
    confluent-update: (external-update (internal-update state) 0 mail)
              output: {}
        time-advance: infinity
   network-structure: []"
  [name [& fields] & specs]
  (let [all-atomic-syms    #{'internal-update 'external-update 'confluent-update 'output 'time-advance}
        all-exec-syms      #{'structure-changes}
        given-atomic-specs (filter (comp all-atomic-syms first) specs)
        given-exec-specs   (filter (comp all-exec-syms first) specs)
        given-atomic-syms  (set (map first given-atomic-specs))
        given-exec-syms    (set (map first given-exec-specs))
        atomic-specs       (cond-> given-atomic-specs
                             (not (contains? given-atomic-syms 'internal-update)) (conj `(internal-update [state#] state#))
                             (not (contains? given-atomic-syms 'external-update)) (conj `(external-update [state# elapsed# mail#] state#))
                             (not (contains? given-atomic-syms 'output))          (conj `(output [state#] {}))
                             (not (contains? given-atomic-syms 'time-advance))    (conj `(time-advance [state#] h/infinity)))
        ;; confluent-update update depends on internal-update and external-update.
        atomic-specs (cond-> atomic-specs
                       (not (contains? given-atomic-syms 'confluent-update))
                       (conj `(confluent-update [state# mail#]
                                (-> (internal-update state#)
                                    (external-update h/zero mail#)))))
        exec-specs   (cond-> given-exec-specs
                       (not (contains? given-exec-syms 'structure-changes))
                       (conj `(structure-changes [state#] [])))]
   `(defrecord ~name [~@fields]
      AtomicModel
      ~@atomic-specs
      ExecutiveModel
      ~@exec-specs)))

(defn executive-model? [x] (satisfies? ExecutiveModel x))
