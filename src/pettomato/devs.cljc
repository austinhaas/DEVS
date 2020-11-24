(ns pettomato.devs
  "This is my implementation, based loosely on the literature, and true to the
  logic, but functional and efficient."
  (:refer-clojure :exclude [run])
  (:require
   [clojure.set :refer [subset?]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.log :as log]))

;; This can be confusing, because there are two types of graphs:

;; 1. The model hierarchy, where every model can be a network composed of
;; sub-models (which can also be networks).

;; 2. The connection topology.

;; Also, networks aren't completely realized in the implementation model. For
;; example, networks don't have state and they don't send or receive messages. A
;; connection may be made between a model and it's containing network, and that
;; network may connect to another model. That just means that the internal model
;; will send messages directly to the receiving model; the containing network is
;; just an abstraction.

;; Performance of message routing depends on the model. The simple, factored
;; system we have now is compact. We might assume that messages that cross
;; networks are relatively rare.

;;------------------------------------------------------------------------------
;; Models

;; Consider changing these to records, and adding an interface that is like the
;; one that takes maps, but also validates and applies default values. It might
;; be faster than using a hash to lookup the same fields over and over again in
;; a map.

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
   time-advance-fn
   network-structure-fn]
  (assert (and (sequential? initial-total-state)
               (= 2 (count initial-total-state))
               (number? (second initial-total-state))))
  (assert (or (nil? internal-update-fn) (ifn? internal-update-fn)))
  (assert (or (nil? external-update-fn) (ifn? external-update-fn)))
  (assert (or (nil? confluent-update-fn) (ifn? confluent-update-fn)))
  (assert (or (nil? output-fn) (ifn? output-fn)))
  (assert (or (nil? time-advance-fn)  (ifn? time-advance-fn)))
  (assert (or (nil? network-structure-fn)  (ifn? network-structure-fn)))
  {:initial-total-state initial-total-state
   :internal-update     internal-update-fn
   :external-update     external-update-fn
   :confluent-update    (or confluent-update-fn
                            (fn [s x] (external-update-fn (internal-update-fn s) 0 x)))
   :output              output-fn
   :time-advance        time-advance-fn
   :network-structure   (or network-structure-fn
                            (constantly nil))})

(defn atomic-model? [model]
  (and (map? model)
       (subset? #{:initial-total-state
                  :internal-update
                  :external-update
                  :confluent-update
                  :output
                  :time-advance
                  :network-structure}
                (set (keys model)))))

(defn network-model [models routes]
  {:models models
   :routes routes})

(defn network-model? [model]
  (and (map? model)
       (subset? #{:models :routes}
                (set (keys model)))))

;;------------------------------------------------------------------------------

(def ^{:dynamic true :private true} *path*
  "Bound to the path to the current model in the network hierarchy."
  ())

(def ^:dynamic *sim-time* nil)

;;------------------------------------------------------------------------------

(def ^:dynamic *trace* false)
(def ^{:dynamic true :private true} *indent* 0)

(defn- pad-left
  "n - min string length of result
   c - char to add to the left
   s - string to add to"
  [n c s]
  (assert (char? c))
  (if (< (count s) n)
    (recur n c (str c s))
    s))

(defn trace [& args]
  (when *trace*
    (apply log/infof
           (str "[" (pad-left 5 \  (str *sim-time*)) "]"
                " " (str (vec (reverse *path*)))
                " " (apply str (repeat *indent* \ ))
                (first args))
           (rest args))))

(defn trace-mail [label mail]
  (when *trace*
    ;;(trace (str "+" (apply str (repeat (+ 2 (count label)) \-)) "+"))
    (trace "%s" label)
    ;;(trace (str "+" (apply str (repeat (+ 2 (count label)) \-)) "+"))
    (doseq [[k vs] mail]
      (trace " %s -> %s" (vec (reverse k)) (vec vs)))))

(defn trace-network-structure-message [name msg]
  (trace "%s" name)
  (case (first msg)
    :add-model  (let [[_ name model] msg] (trace (vec (butlast msg))))
    :rem-model  (let [[_ name]       msg] (trace msg))
    :connect    (let [[_ route]      msg] (trace msg))
    :disconnect (let [[_ route]      msg] (trace msg))))

;;------------------------------------------------------------------------------

(def ^:private empty-pkg
  {:state  {}                  ;; name -> state, where state = {:model :state :tl :tn}
   :routes {}
   :queue  (pq/priority-queue) ;; tn -> set of names
   })

(defn- connect
  [pkg parent [snd-name snd-port rcv-name rcv-port f]]
  (trace "connect: %s" [snd-name snd-port rcv-name rcv-port f])
  (let [snd-path (if (= snd-name :network)
                   parent
                   (cons snd-name parent))
        rcv-path (if (= rcv-name :network)
                   parent
                   (cons rcv-name parent))]
    (update-in pkg [:routes snd-path snd-port rcv-path rcv-port] (fnil conj #{}) f)))

(defn- disconnect
  [pkg parent [snd-name snd-port rcv-name rcv-port f]]
  (trace "disconnect: %s" [snd-name snd-port rcv-name rcv-port f])
  (let [snd-path (if (= snd-name :network)
                   parent
                   (cons snd-name parent))
        rcv-path (if (= rcv-name :network)
                   parent
                   (cons rcv-name parent))]
    ;; TODO: Prune dead branches.
    (update-in pkg [:routes snd-path snd-port rcv-path rcv-port] disj f)))

(declare add-model
         rem-model)

(defn- add-atomic-model [pkg parent name model t]
  (trace "add-atomic-model: %s" name)
  (let [[s e] (:initial-total-state model)
        tl    (- t e)
        tn    (+ tl ((:time-advance model) s))]
    (-> pkg
        (update :state assoc name {:parent parent
                                   :model  model
                                   :state  s
                                   :tl     tl
                                   :tn     tn})
        (update :queue pq/insert tn name))))

(defn- add-network-model [pkg parent name model t]
  (trace "add-network-model: %s" name)
  (as-> pkg pkg
    (update pkg :state assoc name {:parent parent
                                   :model  model})
    (reduce-kv (fn [pkg name' model]
                 ;; Recursive step.
                 (add-model pkg name name' model t))
               pkg
               (:models model))
    (reduce #(connect %1 name %2)
            pkg
            (:routes model))))

(defn- add-model [pkg parent name model t]
  ;;(trace "add-model: %s" name)
  (let [name' (cons name parent)]
   (cond
     (atomic-model?  model) (add-atomic-model pkg parent name' model t)
     (network-model? model) (add-network-model pkg parent name' model t)
     :else                  (throw (ex-info "Unknown model type." {:parent parent
                                                                   :name   name})))))

(defn- rem-atomic-model [pkg parent name]
  (trace "rem-atomic-model: %s" name)
  (let [tn (get-in pkg [:state name :tn])]
    (-> pkg
        (update :state dissoc name)
        (update :queue pq/delete tn name))))

(defn- rem-network-model [pkg parent name]
  (trace "rem-network-model: %s" name)
  (let [model (get-in pkg [:state name :model])]
    (as-> pkg pkg
      (reduce #(disconnect %1 name %2)
              pkg
             (:routes model))
      (reduce-kv (fn [pkg name' model]
                   ;; Recursive step.
                   (rem-model pkg name name'))
                 pkg
                 (:models model))
      (update pkg :state dissoc name))))

(defn- rem-model [pkg parent name]
  (trace "rem-model: %s %s" parent name)
  (let [name' (cons name parent)
        model (get-in pkg [:state name' :model])]
    (cond
      (atomic-model?  model) (rem-atomic-model  pkg parent name')
      (network-model? model) (rem-network-model pkg parent name')
      :else                  (throw (ex-info "Unknown model type." {:parent parent
                                                                    :name   name})))))

;;---

(defn- route
  "mail - snd-name->snd-port->vs

  Returns rcv-name->rcv-port->vs."
  [routes mail]
  ;;(trace "route: %s" mail)
  (loop [out (for [[snd-name snd-port->vs] mail
                   [snd-port vs]           snd-port->vs]
               [snd-name snd-port vs])
         in  []]
    (if (empty? out)
      ;; Convert the list of messages into a trie.
      (reduce (fn [m [rcv-name rcv-port vs]]
                (update-in m [rcv-name rcv-port] into vs))
              {}
              in)
      (let [xs (for [[snd-name snd-port vs]  out
                     [rcv-name rcv-port->fs] (get-in routes [snd-name snd-port])
                     [rcv-port fs]           rcv-port->fs
                     f                       fs]
                 [rcv-name rcv-port (map f vs)])]
        (recur xs (concat in xs))))))

#_
(route {:a {:x {:b {:y [identity]}
                :c {:y [identity]}}}
        :c {:y {:d {:x [identity]}}}
        :d {:x {:e {:y [identity]}}}}
       {:a {:x [100]}})

(defn- apply-network-structure-message [pkg parent msg t]
  (case (first msg)
    :add-model  (let [[_ name model] msg] (add-model  pkg parent name model t))
    :rem-model  (let [[_ name]       msg] (rem-model  pkg parent name))
    :connect    (let [[_ route]      msg] (connect    pkg parent route))
    :disconnect (let [[_ route]      msg] (disconnect pkg parent route))))

(defn- apply-network-structure-messages [pkg parent messages t]
  (reduce #(apply-network-structure-message %1 parent %2 t) pkg messages))

(defn- transition [state mail t]
  (let [{:keys [model state tl tn]} state]
    (let [state (if (empty? mail)
                  (do (trace "internal-update")
                      ((:internal-update model) state))
                  (if (= t tn)
                    (do (trace "confluent-update")
                        ((:confluent-update model) state mail))
                    (do (trace "external-update")
                        ((:external-update model) state (- t tl) mail))))
          tl    t
          tn    (+ tl ((:time-advance model) state))]
      {:model model
       :state state
       :tl    tl
       :tn    tn})))

;;------------------------------------------------------------------------------

(defn collect-mail [m]
  (reduce-kv (fn [m name {:keys [model state]}]
               (binding [*path* name]
                 (let [xs ((:output model) state)]
                   (if (seq xs)
                     (assoc m name xs)
                     m))))
             {}
             m))

(defn collect-structure-changes [m]
  ;; TODO: Might be more efficient to not use a map.
  (reduce-kv (fn [m name {:keys [model state]}]
               (binding [*path* name]
                 (let [xs ((:network-structure model) state)]
                   (if (seq xs)
                     (assoc m name xs)
                     m))))
             {}
             m))

(defn apply-transitions [m mail-in t]
  (reduce-kv (fn [m name state]
               (binding [*path* name]
                 (let [mail (get mail-in name)]
                   (assert state (str "No state found for " name))
                   (assoc m name (transition state mail t)))))
             {}
             m))

(defn step [{:keys [state routes queue] :as pkg} t]
  (binding [*sim-time* t]
    (trace "*** step *********************************************")
    (let [imminent       (pq/peek queue)
          _              (trace "imminent: %s" (mapv (comp vec reverse) imminent))
          ;; ----- Collect mail -----
          _              (trace "--- Collect mail ------------------------------")
          outbound-mail  (collect-mail (select-keys state imminent))
          _              (trace-mail "outbound-mail" outbound-mail)
          inbound-mail   (route routes outbound-mail)
          _              (trace-mail "inbound-mail" inbound-mail)
          atomic?        (fn [name] (atomic-model? (get-in state [name :model])))
          int-mail       (into {} (filter (comp atomic? first) inbound-mail))
          _              (trace-mail "int-mail" int-mail)
          ext-mail       (get inbound-mail [:root])
          _              (trace-mail "ext-mail" (when (seq ext-mail) {[:root] ext-mail}))
          activated      (select-keys state (into imminent (keys int-mail)))
          _              (trace "activated: %s" (mapv (comp vec reverse) (keys activated)))
          ;; ----- Collect network structure change messages -----
          _              (trace "--- Collect network structure change messages -")
          struct-changes (collect-structure-changes activated)
          _              (trace "struct-changes")
          _              (doseq [[name msgs] struct-changes
                                 msg         msgs]
                           (trace-network-structure-message name msg))
          ;; ----- Update models -----
          _              (trace "--- Update models -----------------------------")
          state-delta    (apply-transitions activated int-mail t)
          state'         (merge state state-delta)
          queue'         (pq/change-priority* queue (for [name (keys activated)]
                                                      [(get-in state [name :tn])
                                                       name
                                                       (get-in state' [name :tn])]))
          pkg'           (assoc pkg :state state' :queue queue')
          ;; ----- Update network structure -----
          _              (trace "--- Update network structure ------------------")
          ;; Network structure messages must be processed bottom-up in the
          ;; model hierarchy.
          struct-changes (sort-by (comp count first) struct-changes)
          _              (trace "struct-changes (sorted)")
          _              (doseq [[name msgs] struct-changes
                                 msg         msgs]
                           (trace-network-structure-message name msg))
          pkg'           (reduce (fn [pkg [name vs]]
                                   (let [parent (rest name)]
                                     ;; TODO: Lookup parent instead.
                                     (apply-network-structure-messages pkg parent vs t)))
                                 pkg'
                                 struct-changes)]
      [pkg' ext-mail])))

(defn run
  "Run a simulation from start-time (inclusive) to end-time (exclusive). If
  end-time is not provided, it defaults to infinity, and the simulation will run
  until the simulator returns infinity as its time-of-next-update, which means
  that it will never have another event, so it is safe to quit.

  Returns a seq of [timestamp mail]."
  ([model start-time]
   (run model start-time infinity))
  ([model start-time end-time]
   (binding [*sim-time* start-time]
     (loop [pkg (add-model empty-pkg nil :root model start-time)
            out []]
       (let [t (pq/peek-key (:queue pkg))]
         (if (and t (< t end-time))
           (let [[pkg' out'] (step pkg t)]
             (recur pkg' (if (seq out')
                           (conj out [t out'])
                           out)))
           out))))))
