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

;;------------------------------------------------------------------------------

(def ^:private empty-pkg
  {:state  {}                  ;; name -> state, where state = {:model :state :tl :tn}
   :routes {}
   :queue  (pq/priority-queue) ;; tn -> set of names
   })

(defn- add-atomic-model [pkg name model t]
  (trace "add-atomic-model: %s" name)
  (let [[s e] (:initial-total-state model)
        tl    (- t e)
        tn    (+ tl ((:time-advance model) s))
        path  (cons name *path*)]
    (-> pkg
        (update :state assoc path {:model model :state s :tl tl :tn tn})
        (update :queue pq/insert tn path))))

(declare add-model)

(defn- add-network-model [pkg name model t]
  (trace "add-network-model: %s" name)
  (binding [*path* (cons name *path*)]
    (as-> pkg pkg
      (reduce-kv (fn [pkg name model]
                   ;; Recursive step.
                   (add-model pkg name model t))
                 pkg
                 (:models model))
      (reduce connect
              pkg
              (:routes model)))))

(defn- add-model [pkg name model t]
  ;;(trace "add-model: %s" name)
  (cond
    (atomic-model?  model) (add-atomic-model pkg name model t)
    (network-model? model) (add-network-model pkg name model t)
    :else                  (throw (ex-info "Unknown model type." {:name name}))))

;; TODO: Handle networks.
(defn- rem-model [pkg name]
  (trace "rem-model: %s" name)
  (let [path (cons name *path*)
        tn   (get-in pkg [:state path :tn])]
    (-> pkg
        (update :models dissoc path)
        (update :queue pq/delete tn path))))

(defn- connect
  [pkg [snd-name snd-port rcv-name rcv-port f]]
  (trace "connect: %s" [snd-name snd-port rcv-name rcv-port f])
  (let [snd-path (if (= snd-name :network)
                   *path*
                   (cons snd-name *path*))
        rcv-path (if (= rcv-name :network)
                   *path*
                   (cons rcv-name *path*))]
    (update-in pkg [:routes snd-path snd-port rcv-path rcv-port] (fnil conj #{}) f)))

(defn- disconnect
  [pkg [snd-name snd-port rcv-name rcv-port f]]
  (trace "disconnect: %s" [snd-name snd-port rcv-name rcv-port f])
  (let [snd-path (if (= snd-name :network)
                   *path*
                   (cons snd-name *path*))
        rcv-path (if (= rcv-name :network)
                   *path*
                   (cons rcv-name *path*))]
    ;; TODO: Prune dead branches.
    (update-in pkg [:routes snd-path snd-port rcv-path rcv-port] disj f)))

(defn- route
  "mail - snd-name->snd-port->vs

  Returns rcv-name->rcv-port->vs."
  [routes terminal? mail]
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
      (let [in'         (for [[snd-name snd-port vs]  out
                              [rcv-name rcv-port->fs] (get-in routes [snd-name snd-port])
                              [rcv-port fs]           rcv-port->fs
                              f                       fs]
                          [rcv-name rcv-port (map f vs)])
            {in' true
             net false} (group-by (comp terminal? first) in')]
        (recur net (concat in in'))))))

(defn- apply-network-structure-message [pkg msg t]
  (case (first msg)
    :add-model  (let [[_ name model] msg] (trace (vec (butlast msg))) (add-model pkg name model t))
    :rem-model  (let [[_ name]       msg] (trace msg)                 (rem-model pkg name))
    :connect    (let [[_ route]      msg] (trace msg)                 (connect pkg route))
    :disconnect (let [[_ route]      msg] (trace msg)                 (disconnect pkg route))))

(defn- apply-network-structure-messages [pkg messages t]
  (reduce #(apply-network-structure-message %1 %2 t) pkg messages))

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
          routes         (:routes pkg)
          atomic?        (fn [name] (contains? state name))
          root?          (fn [name] (= [:root] name))
          terminal?      (some-fn atomic? root?)
          inbound-mail   (route routes terminal? outbound-mail)
          _              (trace-mail "inbound-mail" inbound-mail)
          int-mail       (dissoc inbound-mail [:root])
          _              (trace-mail "int-mail" int-mail)
          ext-mail       (get inbound-mail [:root])
          _              (trace-mail "ext-mail" (when (seq ext-mail) {[:root] ext-mail}))
          activated      (select-keys state (into imminent (keys int-mail)))
          _              (trace "activated: %s" (mapv (comp vec reverse) (keys activated)))
          ;; ----- Collect network structure change messages -----
          _              (trace "--- Collect network structure change messages -")
          struct-changes (collect-structure-changes activated)
          _              (trace "struct-changes: %s" (count struct-changes))
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
          pkg'           (reduce (fn [pkg [name vs]]
                                   (binding [*path* (rest name)]
                                     (apply-network-structure-messages pkg vs t)))
                                 pkg'
                                 struct-changes)]
      [pkg' ext-mail])))

(defn run
  "Run a simulation from start-time (inclusive) to end-time (exclusive). If
  end-time is not provided, it defaults to infinity, and the simulation will run
  until the simulator returns infinity as its time-of-next-update, which means
  that it will never have another event, so it is safe to quit."
  ([model start-time]
   (run model start-time infinity))
  ([model start-time end-time]
   (binding [*sim-time* start-time]
     (loop [pkg (add-model empty-pkg :root model start-time)
            out []]
       (let [t (pq/peek-key (:queue pkg))]
         (if (and t (< t end-time))
           (let [[pkg' out'] (step pkg t)]
             (recur pkg' (if (seq out')
                           (conj out [t out'])
                           out)))
           out))))))
