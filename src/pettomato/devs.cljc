(ns pettomato.devs
  #?(:cljs (:require-macros [pettomato.devs :refer [def-executive-model]]))
  (:require
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   #?(:cljs [goog.async.Delay :as gdelay])
   #?(:cljs [goog.string :as gstring :refer [format]])
   #?(:cljs [goog.string.format])
   [pettomato.devs.clock :as clock]
   [pettomato.devs.lib.coll :refer [prune]]
   [pettomato.devs.lib.date :as date]
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.print :as print]
   [pettomato.devs.lib.priority-queue :as pq]
   [pettomato.devs.mail :as mail]
   [pettomato.devs.trace :as trace])
  #?(:clj (:import [java.io Writer])))

;;------------------------------------------------------------------------------
;; Atomic Model

(defprotocol AtomicModel
  "A protocol for atomic models."
  (internal-update [state]
    "The internal state-transition function. Takes a state and returns a
  new state. Invoked when the model is imminent.")
  (external-update [state elapsed mail]
    "The external state-transition function. Takes a state, the time
  elapsed in the state, and a bag of messages, and returns a new
  state. Invoked when the model has incoming messages.")
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

;;------------------------------------------------------------------------------
;; Executive Model

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

(def-executive-model SimpleExec [structure-changes]
  (internal-update [state] (update state :structure-changes empty))
  (external-update [state elapsed mail] (update state :structure-changes into (:in mail)))
  (time-advance [state] (if (seq structure-changes) h/epsilon h/infinity))
  (structure-changes [state] structure-changes))

(defn simple-executive
  "A network executive that accepts structure change messages on
  port :in and provides them as-is to the network simulator."
  []
  (->SimpleExec []))

(def-executive-model StaticExec [])

(defn static-executive []
  (->StaticExec))

;;------------------------------------------------------------------------------
;; Network Model

(defrecord NetworkModel [executive-id executive-model models routes])

(defn validate-network-model!
  "Throws an exception if model is invalid."
  [model]
  (let [[executive-model elapsed] (:executive-model model)
        executive-id              (:executive-id model)
        {:keys [models routes]}   model
        models                    (assoc models executive-id executive-model)]
    (ex-assert (executive-model? executive-model)
               "Executive must be an executive model.")
    (ex-assert (atomic-model? executive-model)
               "Executive must be an atomic model.")
    (ex-assert (and (h/hyperreal? elapsed)
                    (h/<= h/zero elapsed)
                    (h/< elapsed (time-advance executive-model)))
               "Invalid elapsed time for network executive."
               {:elapsed elapsed})
    ;; All models in routes must appear in models (except for :network).
    (let [keys-in-routes (-> (mapcat (fn [[sk _ rk _ _]] [sk rk])
                                     routes)
                             set
                             (disj :network))
          keys-in-models (-> models
                             keys
                             set)]
      (ex-assert (set/subset? keys-in-routes keys-in-models)
                 "All models in routes must appear in models (except for :network)."
                 {:unknown-models (set/difference keys-in-routes keys-in-models)}))
    ;; A model cannot use the same port for both input and output.
    (let [input-ports  (set (for [[sk sp _ _ _] routes] [sk sp]))
          output-ports (set (for [[_ _ rk rp _] routes] [rk rp]))]
      (ex-assert (empty? (set/intersection input-ports output-ports))
                 "A model cannot use the same port for both input and output."))
    ;; A network input port cannot connect directly to a network output port.
    (let [pass-thru-routes (filter (fn [[sk _ rk _ _]] (= sk rk))
                                   routes)]
      (ex-assert (empty? pass-thru-routes)
                 "A network input port cannot connect directly to a network output port."
                 {:bad-routes pass-thru-routes})))
  nil)

(defn network-model
  "Construct a network model. Optionally, an initial set of models and
  routes may be supplied."
  ([executive-id executive-model]
   (network-model executive-id executive-model {} []))
  ([executive-id executive-model models routes]
   (let [model (->NetworkModel executive-id executive-model models routes)]
     (validate-network-model! model)
     model)))

(defn network-model? [x] (instance? NetworkModel x))

(defn simple-network-model
  "A network model with a simple-executive."
  ([executive-id]
   (simple-network-model executive-id [] []))
  ([executive-id models routes]
   (network-model executive-id [(simple-executive) h/zero] models routes)))

(defn static-network-model
  "A network model for static networks. The model has a network
  executive, but it is inaccessible and does nothing."
  [models routes]
  (network-model (gensym) [(static-executive) h/zero] models routes))

;;------------------------------------------------------------------------------
;; Simulator

(defprotocol Simulator
  (initialize         [sim t]
    "Initialize sim at sim-time t. Returns a new sim.")
  (collect-mail       [sim t]
    "Get outgoing mail for sim at sim-time t. Returns mail,
where mail = p->vs.")
  (transition         [sim mail t]
    "Execute a state transition at sim-time t, with mail = p->vs. Returns
a new sim.")
  (time-of-last-event [sim]
    "Returns the last time sim was updated.")
  (time-of-next-event [sim]
    "Returns the scheduled time of the next sim internal update, i.e., the maximum
time sim will remain in its current state, i.e., the time of sim's next update
if it doesn't receive any external messages before then."))

(defn step
  "Advance sim to time t and send it mail at that time.

  If only sim is provided, advance sim to the time of its next event.

  Returns [sim mail].

  Throws an exception if:

    - t <= time-of-last-event.
    - t > time-of-next-event.
    - t is not provided and time-of-next-event is infinite.
    - t < time-of-next-event and mail is empty.

  This is a low-level function. `step*` is a higher-level function
  with a friendlier interface."
  ([sim]
   (let [tn (time-of-next-event sim)]
     (ex-assert (not (h/infinite? tn))
                "Cannot step sim, because time-of-next-event is infinite.")
     (let [mail (collect-mail sim tn)
           sim  (transition sim {} tn)]
       [sim mail])))
  ([sim t mail]
   (let [tl (time-of-last-event sim)
         tn (time-of-next-event sim)]
     (ex-assert (h/< tl t) "Synchronization error.")
     (ex-assert (h/<= t tn) "Synchronization error.")
     (ex-assert (or (h/= t tn)
                    (seq mail))
                "Sim must be imminent or receiving mail.")
     (let [mail' (if (h/= t tn)
                   (collect-mail sim t)
                   {})
           sim   (transition sim mail t)]
       [sim mail']))))

(defn step*
  "Repeatedly step sim. Returns [sim mail-log].

  If `t` is provided, the sim will not be stepped past t.

  If `t` and `mail` are provided, the sim will stepped up to `t` and
  then at `t` the sim will be stepped with `mail` as input."
  ;; TODO: Add additional optional parameters like sim-end-fn and
  ;; mail-end-fn that cause the sim to terminate when they return
  ;; true. We might want to terminate when a particular message is
  ;; returned, for example.

  ;; Implementation note: This function was defined with multiple
  ;; arities instead of optional keywords to enforce the only
  ;; meaningful use cases. For example, if `mail` could be supplied
  ;; without `t`, that would mean, send the mail whenever the next
  ;; internal event occurs, and I can't imagine a scenario where that
  ;; would be a good idea.

  ([sim] ; Step until sim's time-of-next-event is infinite.
   (step* sim h/infinity))
  ([sim t] ; Step until sim's time-of-next-event is > t.
   (loop [sim      sim
          mail-log []]
     (let [tn (time-of-next-event sim)]
       (if (or (h/infinite? tn)
               (h/< t tn))
         [sim mail-log]
         (let [[sim mail] (step sim)
               mail-log   (if (seq mail)
                            (conj mail-log [(time-of-last-event sim) mail])
                            mail-log)]
           (recur sim mail-log))))))
  ([sim t mail] ; Step to t and send mail.
   (loop [sim      sim
          mail-log []]
     (let [tn (time-of-next-event sim)]
       (if (or (h/infinite? tn)
               (h/<= t tn))
         (let [[sim mail] (step sim t mail)
               mail-log   (if (seq mail)
                            (conj mail-log [(time-of-last-event sim) mail])
                            mail-log)]
           [sim mail-log])
         (let [[sim mail] (step sim)
               mail-log   (if (seq mail)
                            (conj mail-log [(time-of-last-event sim) mail])
                            mail-log)]
           (recur sim mail-log)))))))

(defn step**
  "Repeatedly step sim and incorporate a mail-log of mail to send to sim
  at the designated times.

  Returns [sim mail-log].

  Optional parameters:

    end - Simulation end time (inclusive, unless infinity).
          Default: (hyperreal) infinity.

    mail-log - A seq of [time mail].

  Prefer `step*`, unless you really need to send multiple mail
  messages in one call."
  [sim & {:keys [end mail-log]
          :or   {end      h/infinity
                 mail-log []}}]
  (loop [sim        sim
         input-log  (take-while #(h/<= (first %) end) mail-log)
         output-log []]
    (if (seq input-log)
      (let [[t mail]       (first input-log)
            [sim mail-log] (step* sim t mail)]
        (recur sim (rest input-log) (into output-log mail-log)))
      (let [[sim mail-log] (step* sim end)]
        [sim (into output-log mail-log)]))))

;;------------------------------------------------------------------------------
;; Atomic Simulator

(defrecord AtomicSimulator [initial-state initial-elapsed state tl tn]
  Simulator
  (initialize [sim t]
    (trace/with-initialize [sim t]
      (ex-assert (h/<= h/zero initial-elapsed)
                 "initial-elapsed must be <= 0"
                 {:initial-elapsed initial-elapsed})
      (let [tl (h/- t initial-elapsed)
            tn (h/+ tl (time-advance initial-state))]
        (ex-assert (h/<= tl t)
                   "NIA violation"
                   {:tl tl :t t})
        (ex-assert (h/< t tn)
                   "NIA violation"
                   {:t t :tn tn})
        (assoc sim :state initial-state :tl tl :tn tn))))
  (collect-mail [sim t]
    (trace/with-collect-mail [sim t]
      (ex-assert (h/= t tn) "synchronization error" {:t t :tn tn})
      (output state)))
  (transition [sim mail t]
    (trace/with-transition [sim mail t]
      (ex-assert (h/< tl t)
                 "synchronization error"
                 {:tl tl :t t :tn tn})
      (ex-assert (h/<= t tn)
                 "synchronization error"
                 {:tl tl :t t :tn tn})
      (ex-assert (or (h/= t tn) (seq mail))
                 "Illegal state for transition; sim is not imminent nor receiving mail."
                 {:tl tl :t t :tn tn :mail-count (count mail)})
      (let [state (if (h/= t tn)
                    (if (seq mail)
                      (confluent-update state mail)
                      (internal-update state))
                    (external-update state (h/- t tl) mail))
            tl    t
            tn    (h/+ t (time-advance state))]
        (ex-assert (h/< tl tn)
                   "tn must be greater than tl."
                   {:tl tl :tn tn})
        (assoc sim :state state :tl tl :tn tn))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn)
  Object
  (toString [sim]
    #?(:clj  (format "#pettomato.devs.simulators.AtomicSimulator<0x%x>{}"
                     (System/identityHashCode sim))
       :cljs (format "#pettomato.devs.simulators.AtomicSimulator<%s>{}"
                     (.toString (hash sim) 16)))))

#?(:clj (print/add-print-handlers-clj AtomicSimulator)
   :cljs (print/add-print-handlers-cljs AtomicSimulator))

(defn atomic-simulator
  "Wrap an atomic model in an atomic simulator.

  Args:
    model - An atomic model.

  Returns:
    A simulator."
  [model & {:keys [elapsed]
            :or   {elapsed h/zero}}]
  (ex-assert (atomic-model? model))
  (map->AtomicSimulator {:initial-state   model
                         :initial-elapsed elapsed}))

;;------------------------------------------------------------------------------
;; Executive Simulator

(defprotocol IExecutiveSimulator
  (get-structure-changes [sim t]))

(defrecord ExecutiveSimulator [initial-state initial-elapsed state tl tn]
  Simulator
  (initialize [sim t]
    (trace/with-initialize [sim t]
      (ex-assert (h/<= h/zero initial-elapsed)
                 "initial-elapsed must be <= 0"
                 {:initial-elapsed initial-elapsed})
      (let [tl (h/- t initial-elapsed)
            tn (h/+ tl (time-advance initial-state))]
        (ex-assert (h/<= tl t)
                   "NIA violation"
                   {:tl tl :t t})
        (ex-assert (h/< t tn)
                   "NIA violation"
                   {:t t :tn tn})
        (assoc sim :state initial-state :tl tl :tn tn))))
  (collect-mail [sim t]
    (trace/with-collect-mail [sim t]
      (ex-assert (h/= t tn) "synchronization error" {:t t :tn tn})
      (output state)))
  (transition [sim mail t]
    (trace/with-transition [sim mail t]
      (ex-assert (h/< tl t)
                 "synchronization error"
                 {:tl tl :t t :tn tn})
      (ex-assert (h/<= t tn)
                 "synchronization error"
                 {:tl tl :t t :tn tn})
      (ex-assert (or (h/= t tn) (seq mail))
                 "Illegal state for transition; sim is not imminent nor receiving mail."
                 {:tl tl :t t :tn tn :mail-count (count mail)})
      (let [state (if (h/= t tn)
                    (if (seq mail)
                      (confluent-update state mail)
                      (internal-update state))
                    (external-update state (h/- t tl) mail))
            tl    t
            tn    (h/+ t (time-advance state))]
        (ex-assert (h/< tl tn)
                   "tn must be greater than tl."
                   {:tl tl :tn tn})
        (assoc sim :state state :tl tl :tn tn))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn)
  IExecutiveSimulator
  (get-structure-changes [sim t]
    (ex-assert (h/= t tn) "synchronization error" {:t t :tn tn})
    (structure-changes state))
  Object
  (toString [sim]
    #?(:clj  (format "#pettomato.devs.simulators.ExecutiveSimulator<0x%x>{}"
                     (System/identityHashCode sim))
       :cljs (format "#pettomato.devs.simulators.ExecutiveSimulator<%s>{}"
                     (.toString (hash sim) 16)))))

#?(:clj (print/add-print-handlers-clj ExecutiveSimulator)
   :cljs (print/add-print-handlers-cljs ExecutiveSimulator))

(defn executive-simulator
  "Wrap a network executive model in a network executive simulator.

  Args:
    model - A network executive model.

  Returns:
    A simulator."
  [model & {:keys [elapsed]
            :or   {elapsed h/zero}}]
  (ex-assert (executive-model? model))
  (map->ExecutiveSimulator {:initial-state   model
                            :initial-elapsed elapsed}))

;;------------------------------------------------------------------------------
;; Network Simulator

(declare network-simulator)

(defn- find-simulator
  "Find an appropriate simulator for a model.

  id - The id that will be assigned to the model.

  model - The model.

  Returns a simulator."
  [id model]
  (cond
    (executive-model? model) executive-simulator
    (atomic-model?  model)   atomic-simulator
    (network-model? model)   network-simulator
    :else                    (throw (ex-info "Unknown model type." {:id id}))))

(defn- add-model [parent-sim id [model elapsed] t]
  ;; Adding a model at time t means that the state of the model is
  ;; already determined at time t. In other words, we do not add a
  ;; model at time t, and then invoke a transition to bring it up to
  ;; speed at time t.
  (trace/add-model id [model elapsed])
  (ex-assert (not (contains? (:id->sim parent-sim) id))
             "duplicate id"
             {:id id})
  (let [simulator (find-simulator id model)
        sim       (binding [trace/*context* (update trace/*context* :path conj id)]
                    (-> model (simulator :elapsed elapsed) (initialize t)))
        tl        (time-of-last-event sim)
        tn        (time-of-next-event sim)]
    (ex-assert (h/<= tl t))
    (ex-assert (h/< t tn))
    (as-> parent-sim s
      (update s :id->sim assoc id sim)
      (update s :queue pq/insert tn id)
      (assoc s :tl (h/max (:tl parent-sim) tl))
      (assoc s :tn (h/min (:tn parent-sim) tn)))))

(defn- rem-model [parent-sim id]
  (trace/rem-model id)
  (ex-assert (not= (get-in parent-sim [:model :executive-id]) id)
             "Can't remove the network executive.")
  (let [sim (get-in parent-sim [:id->sim id])]
    (ex-assert sim "id not found" {:id id})
    (as-> parent-sim s
      (update s :id->sim dissoc id)
      (update s :queue pq/delete (time-of-next-event sim) id)
      (assoc  s :tn (pq/peek-key (:queue s))))))

(defn- connect
  "Add a route to the routing table.

  f is optional; defaults to the identity function."
  [parent-sim [sk sp rk rp f]]
  (trace/connect [sk sp rk rp f])
  (let [f (or f identity)]
    (cond
      (= :network sk)
      (update-in parent-sim [:network-input-routes sk sp rk rp] (fnil conj #{}) f)

      (= :network rk)
      (update-in parent-sim [:network-output-routes sk sp rk rp] (fnil conj #{}) f)

      :else
      (update-in parent-sim [:local-routes sk sp rk rp] (fnil conj #{}) f))))

(defn- disconnect
  "Remove a route from the routing table.

  f is optional; defaults to the identity function."
  [parent-sim [sk sp rk rp f]]
  (trace/disconnect [sk sp rk rp f])
  (let [f (or f identity)]
    (cond
      (= :network sk)
      (-> parent-sim
          (update-in [:network-input-routes sk sp rk rp] disj f)
          (update :network-input-routes prune [sk sp rk rp]))

      (= :network rk)
      (-> parent-sim
          (update-in [:network-output-routes sk sp rk rp] disj f)
          (update :network-output-routes prune [sk sp rk rp]))

      :else
      (-> parent-sim
          (update-in [:local-routes sk sp rk rp] disj f)
          (update :local-routes prune [sk sp rk rp])))))

(defn- apply-transition
  "Invoke a transition for a single simulator.

  parent-sim - The network simulator.

  id - The name of the component simulator.

  t - The current sim-time.

  mail - The local mail (p->vs) for the component simulator."
  [parent-sim id t mail]
  (let [sim  (get-in parent-sim [:id->sim id])
        sim' (binding [trace/*context* (update trace/*context* :path conj id)]
               ;; Recursive step.
               (transition sim mail t))
        tn   (time-of-next-event sim) ; Previously scheduled time; (<= t tn).
        tn'  (time-of-next-event sim')]
    (-> parent-sim
        (assoc-in [:id->sim id] sim')
        (update :queue pq/change-priority tn id tn'))))

(defn- apply-transitions
  "Invoke a transition across all component simulators.

  parent-sim - The network simulator.

  t - The current sim-time.

  mail - Inbound mail for this simulator (k->p->vs)."
  [parent-sim t mail]
  ;; This could be parallelized.
  (reduce-kv #(apply-transition %1 %2 t %3)
             parent-sim
             mail))

(defn- apply-network-structure-changes [parent-sim t xs]
  ;; Network structure messages are grouped and processed in a specific order.
  ;; Himmelspach, Jan, and Adelinde M. Uhrmacher. "Processing dynamic PDEVS models."
  ;; The IEEE Computer Society's 12th Annual International Symposium on Modeling, Analysis, and Simulation of Computer and Telecommunications Systems, 2004.
  ;; Section 3.2
  ;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.3385&rep=rep1&type=pdf
  (let [xs         (group-by first xs)
        add-model  (fn [sim [_ id [model elapsed]]] (add-model sim id [model elapsed] t))
        rem-model  (fn [sim [_ id]] (rem-model sim id))
        connect    (fn [sim [_ route]] (connect sim route))
        disconnect (fn [sim [_ route]] (disconnect sim route))]
    (as-> parent-sim parent-sim
      (reduce disconnect parent-sim (:disconnect xs))
      (reduce rem-model  parent-sim (:rem-model  xs))
      (reduce add-model  parent-sim (:add-model  xs))
      (reduce connect    parent-sim (:connect    xs)))))

(defrecord NetworkSimulator [model initial-elapsed
                             id->sim queue
                             local-routes network-input-routes network-output-routes
                             tl tn]
  Simulator
  (initialize [sim t]
    (trace/with-initialize [sim t]
      (let [exec-id    (:executive-id model)
            exec-model (:executive-model model)
            tl         (h/- t initial-elapsed)
            sim        (as-> sim sim
                         (assoc sim
                                :id->sim               {}
                                :local-routes          {}
                                :network-input-routes  {}
                                :network-output-routes {}
                                :queue                 (pq/priority-queue h/comparator)
                                :tl                    tl
                                :tn                    h/infinity)
                         (add-model sim exec-id exec-model tl)
                         (reduce-kv #(add-model %1 %2 %3 tl) sim (:models model))
                         (reduce connect sim (:routes model)))]
        (ex-assert (h/< t (:tn sim)) "tn can't be in the past.")
        sim)))
  (collect-mail [sim t]
    (trace/with-collect-mail [sim t]
      (ex-assert (h/= t tn)
                 "synchronization error"
                 {:t t :tn tn})
      (->> (set/intersection (pq/peek queue) (set (keys network-output-routes))) ; The set of model ids that have network-output-routes could be cached.
           (select-keys id->sim)
           (reduce-kv (fn [m id sim] (assoc m id (binding [trace/*context* (update trace/*context* :path conj id)]
                                                   (collect-mail sim t)))) {})
           (mail/route-mail network-output-routes)
           :network)))
  (transition [sim mail t]
    (trace/with-transition [sim mail t]
      (ex-assert (h/< tl t)
                 "synchronization error"
                 {:tl tl :t t :tn tn})
      (ex-assert (h/<= t tn)
                 "synchronization error"
                 {:tl tl :t t :tn tn})
      (ex-assert (or (h/= t tn) (seq mail))
                 "Illegal state for transition; sim is not imminent nor receiving mail."
                 {:tl tl :t t :tn tn :mail-count (count mail)})
      (let [imminent          (if (h/= t tn) (pq/peek queue) #{})
            imm-mail          (zipmap imminent (repeat {})) ; Transitions are "mail-driven"; imminent sims are primed with an empty bag.
            ;; There could be redundancy here, if a component routes
            ;; output to the network (in collect-mail above) and also
            ;; locally (here).
            local-mail        (->> (set/intersection imminent (set (keys local-routes))) ; The set of model ids that have local-routes could be cached.
                                   (select-keys id->sim)
                                   (reduce-kv (fn [m id sim] (assoc m id (collect-mail sim t))) {})
                                   (mail/route-mail local-routes))
            network-mail      (mail/route-mail network-input-routes {:network mail})
            all-mail          (mail/merge-mail imm-mail local-mail network-mail)
            structure-changes (if (contains? imminent (:executive-id model))
                                (get-structure-changes (get (:id->sim sim) (:executive-id model)) t)
                                [])]
        (ex-assert (h/< tl tn)
                   "tn must be greater than tl."
                   {:tl tl :tn tn})
        (as-> sim s
          (apply-transitions s t all-mail)
          (apply-network-structure-changes s t structure-changes)
          (assoc s
                 :tl t
                 :tn (pq/peek-key (:queue s)))))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(defn network-simulator
  "Wrap a network model in a NetworkSimulator.

  Args:
    model - A network model.

  Returns:
    A NetworkSimulator."
  [model & {:keys [elapsed]
            :or   {elapsed h/zero}}]
  (ex-assert (network-model? model))
  (map->NetworkSimulator {:model model :initial-elapsed elapsed}))

;;------------------------------------------------------------------------------
;; As-Fast-As-Possible (AFAP) Root-Coordinator

(defn afap-root-coordinator
  "Run a simulation \"as fast as possible\".

  Args:
    sim - A simulator.

  Optional keyword args:
    start - Simulation start time (inclusive). Default: (hyperreal) zero.
    end - Simulation end time (inclusive, unless infinity). Default: (hyperreal) infinity.
    input-log - A seq of [sim-time mail] to be used as input to sim.

  Returns:
    mail-log

  \"Analytic simulations typically execute 'as-fast-as-possible,' meaning that
  the simulation attempts to complete its computations as quickly as it
  can. This could mean that the simulator advances faster than real-time (for
  example, it might simulate hours of system behavior in only minutes of elapsed
  time to the user) or that it runs slower than real-time.\"

    - Fujimoto. Parallel and Distributed Simulation Systems. 2000. p. 7."
  [sim & {:keys [start end input-log]
          :or   {start     h/zero
                 end       h/infinity
                 input-log []}}]
  (-> (initialize sim start)
      (step** :end end :mail-log input-log)
      second))

;;------------------------------------------------------------------------------
;; Real-Time Root-Coordinator

;; Locking isn't necessary for CLJS (does it do anything at all?), but
;; it doesn't hurt either (AFAICT), so it's simpler to leave it in.

(def lock #?(:clj (Object.) :cljs (js/Object)))

(defn default-print-mail [mail-log]
  (binding [*print-level*  10
            *print-length* 10]
    (doseq [[t mail] mail-log]
      (log/infof "out> %s %s" t mail))))

(declare start-loop!
         step!
         wall-time-until-next-event)

(defn- start-loop-fn [pkg]
 #?(:clj
    (doto (Thread.
           (fn []
             (try
               (while (not (Thread/interrupted))
                 (let [delta (wall-time-until-next-event pkg)]
                   (if (infinite? delta)
                     (.interrupt (Thread/currentThread))
                     (do (Thread/sleep delta)
                         (step! pkg)))))
               (catch java.lang.InterruptedException e
                 nil))))
      .start)
    :cljs
    (letfn [(special-delay! [f initial-delay]
              (let [handle (atom nil)]
                (letfn [(step [d]
                          (if (infinite? d)
                            (reset! handle nil)
                            (->> (goog.async.Delay. #(step (f)) d)
                                 (reset! handle)
                                 .start)))]
                  (step initial-delay))
                handle))]
      (special-delay!
       #(-> pkg step! wall-time-until-next-event)
       (wall-time-until-next-event pkg)))))

(defn- stop-loop-fn [handle]
 #?(:clj
    (do (doto ^Thread handle .interrupt .join)
        nil)
    :cljs
    (when @handle
      (.stop @handle)
      nil)))

(defn rt-root-coordinator
  "A root-coordinator that is gated by real-time."
  [sim & {:keys [sim-time
                 wall-time-fn
                 paused?
                 scale-factor
                 output-fn
                 start-loop-fn
                 stop-loop-fn]
          :or   {sim-time      h/zero
                 wall-time-fn  date/timestamp
                 paused?       false
                 scale-factor  1.0
                 output-fn     default-print-mail
                 start-loop-fn start-loop-fn
                 stop-loop-fn  stop-loop-fn}}]
  (let [clock (clock/clock :sim-time sim-time
                           :paused? paused?
                           :scale-factor scale-factor
                           :wall-time-fn wall-time-fn)
        sim   (initialize sim (clock/get-sim-time clock))
        rc    (atom
               {:clock         clock
                :sim           sim
                :output-fn     output-fn
                :start-loop-fn start-loop-fn
                :stop-loop-fn  stop-loop-fn
                :loop-handle   nil})]
    (if paused?
      rc
      (start-loop! rc))))

(defn- start-loop! [rc]
  (swap! rc assoc :loop-handle ((:start-loop-fn @rc) rc))
  rc)

(defn- stop-loop! [rc]
  (swap! rc assoc :loop-handle ((:stop-loop-fn @rc) (:loop-handle @rc)))
  rc)

(defn wall-time-until-next-event [rc]
  (let [{:keys [clock sim]} @rc
        next-sim-time       (time-of-next-event sim)
        curr-sim-time       (clock/get-sim-time clock)
        scale-factor        (clock/get-scale-factor clock)]
    (if (h/< curr-sim-time next-sim-time)
      (let [delta (h/- next-sim-time curr-sim-time)]
        (if (h/infinite? delta)
          (h/standard delta)
          (/ (h/standard delta) scale-factor)))
      0)))

(defn step!
  ([rc]
   (locking lock
     (let [{:keys [clock sim output-fn]} @rc
           [sim mail] (step* sim (clock/get-sim-time clock))]
       (swap! rc assoc :sim sim)
       (output-fn mail)
       rc)))
  ([rc mail]
   (locking lock
     (let [{:keys [clock sim output-fn]} @rc
           [sim mail] (step* sim (clock/get-sim-time clock) mail)]
       (swap! rc assoc :sim sim)
       (output-fn mail)
       rc))))

(defn paused? [rc]
  (-> rc deref :clock clock/paused?))

(defn pause! [rc]
  (locking lock
    (when (not (paused? rc))
      (swap! rc update :clock clock/pause)
      (stop-loop! rc))
    rc))

(defn unpause! [rc]
  (locking lock
    (when (paused? rc)
      (swap! rc update :clock clock/unpause)
      (start-loop! rc))
    rc))

(defn send-mail! [rc mail]
  (locking lock
    (stop-loop! rc)
    (step! rc mail)
    (when (not (paused? rc))
      (start-loop! rc))
    rc))

(defn set-scale-factor! [rc scale-factor]
  (locking lock
    (stop-loop! rc)
    (swap! rc update :clock clock/set-scale-factor scale-factor)
    (when (not (paused? rc))
      (start-loop! rc))
    rc))
