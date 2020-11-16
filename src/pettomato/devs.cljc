(ns pettomato.devs
  "This is my implementation, based loosely on the literature, and true to the
  logic, but functional and efficient."
  (:refer-clojure :exclude [run])
  (:require
   [clojure.set :refer [union]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [dissoc-in infinity]]
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
;; Model Protocols

(defprotocol IModel
  "A protocol for an atomic model."
  (initial-total-state [this]                        "Returns [initial-state elapsed]. State can be anything; it is only used by the functions in this protocol.")
  (internal-update     [this state]                  "Returns new state.")
  (external-update     [this state elapsed messages] "Returns new state.")
  (confluent-update    [this state messages]         "Returns new state.")
  (output              [this state]                  "Returns mail, which is a map from port name to a seq of values.")
  (time-advance        [this state]                  "Returns time until the next internal update."))

(defprotocol IExecutive
  "A protocol for an executive model."
  (network-structure-output [this state] "Returns network structure messages."))

(defprotocol INetwork
  "A protocol for a network model."
  (exec-name  [this] "Returns the name of the network executive.")
  (exec-model [this] "Returns the network executive model."))

;;------------------------------------------------------------------------------

(def ^{:dynamic true :private true} *path*
  "Bound to the path to the current model in the network hierarchy."
  [])

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
                " " (str *path*)
                " | " (apply str (repeat *indent* \ ))
                (first args))
           (rest args))))

;;------------------------------------------------------------------------------

(def ^:private empty-pkg
  {:state  {}                  ;; name -> state, where state = {:model :state :tl :tn}
   :routes {}
   :queue  (pq/priority-queue) ;; tn -> set of names
   })

(declare apply-network-structure-messages)

(defn- canonicalize-name
  "Use paths as canonical names for models in a flattened hierarchy. Substitute
  internal :network references with the model's external name."
  [name]
  (if (= :network name)
    *path*
    (conj *path* name)))

(defn- add-atomic-model [pkg name model t]
  (trace "add-atomic-model: %s" name)
  (let [[s e] (initial-total-state model)
        tl    (- t e)
        tn    (+ tl (time-advance model s))
        path  (canonicalize-name name)]
    (-> pkg
        (update :state assoc path {:model model :state s :tl tl :tn tn})
        (update :queue pq/insert tn path))))

(defn- add-executive-model [pkg name model t]
  (trace "add-executive-model: %s" name)
  (let [pkg   (-> pkg
                  (add-atomic-model name model t))
        path  (canonicalize-name name)
        state (get-in pkg [:state path :state])]
    (let [xs (network-structure-output model state)]
      ;; Recursive step.
      (apply-network-structure-messages pkg xs t))))

(defn- add-network-model [pkg name model t]
  (trace "add-network-model: %s" name)
  (binding [*path* (conj *path* name)]
    (-> pkg
        (add-executive-model (exec-name model) (exec-model model) t))))

(defn- add-model [pkg name model t]
  (trace "add-model: %s" name)
  (condp satisfies? model
    IModel     (add-atomic-model    pkg name model t)
    IExecutive (add-executive-model pkg name model t)
    INetwork   (add-network-model   pkg name model t)))

;; TODO: Handle networks and executives.
(defn- rem-model [pkg name]
  (trace "rem-model: %s" name)
  (let [path (canonicalize-name name)
        tn   (get-in pkg [:state path :tn])]
    (-> pkg
        (update :models dissoc path)
        (update :queue pq/delete tn path))))

(defn- connect
  [pkg [snd-name snd-port rcv-name rcv-port f]]
  (trace "connect: %s" [snd-name snd-port rcv-name rcv-port f])
  (let [snd-path (canonicalize-name snd-name)
        rcv-path (canonicalize-name rcv-name)]
    (update-in pkg [:routes snd-path snd-port rcv-path rcv-port] (fnil conj #{}) f)))

(defn- disconnect
  [pkg [snd-name snd-port rcv-name rcv-port f]]
  (trace "disconnect: %s" [snd-name snd-port rcv-name rcv-port f])
  (let [snd-path (canonicalize-name snd-name)
        rcv-path (canonicalize-name rcv-name)]
    ;; TODO: Prune dead branches.
    (update-in pkg [:routes snd-path snd-port rcv-path rcv-port] disj f)))

(defn- route
  "mail - snd-name->snd-port->vs

  Returns rcv-name->rcv-port->vs."
  [pkg mail]
  (trace "route: %s" mail)
  (let [routes    (:routes pkg)
        terminal? (-> (:state pkg)
                      keys
                      set
                      ;; root network
                      (conj [:root]) )]
    (loop [out (for [[snd-name snd-port->vs] mail
                     [snd-port vs]           snd-port->vs]
                 [snd-name snd-port vs])
           in  []]
      (if (empty? out)
        ;; structure
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
               net false} (group-by (comp boolean terminal? first) in')]
          (recur net (concat in in')))))))

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
                  (internal-update model state)
                  (if (= t tn)
                    (confluent-update model state mail)
                    (external-update model state (- t tl) mail)))
          tl    t
          tn    (+ tl (time-advance model state))]
      {:model model
       :state state
       :tl    tl
       :tn    tn})))

;;------------------------------------------------------------------------------

(defn step [{:keys [state routes queue] :as pkg} t]
  (binding [*sim-time* t]
    (trace "step")
    (let [imminent  (pq/peek queue)
          _         (trace "imminent: %s" imminent)
          ;; ----- Collect mail -----
          mail-out  (zipmap imminent (->> imminent
                                          (map (fn [name]
                                                 (binding [*path* name]
                                                   (let [{:keys [model state]} (get state name)]
                                                     (output model state)))))))
          _         (trace "mail-out: %s" mail-out)
          mail-in   (route pkg mail-out)
          _         (trace "mail-in: %s" mail-in)
          ;; TODO: Don't hardcode this.
          receivers (-> (keys mail-in) set (disj [:root]))
          _         (trace "receivers: %s" receivers)
          activated (union imminent receivers)
          _         (trace "activated: %s" activated)
          ;; ----- Update models -----
          updated   (map (fn [name]
                           (binding [*path* name]
                             (let [state (get state name)
                                   mail  (get mail-in name)]
                               (assert state (str "No state found for " name))
                               (transition state mail t))))
                         activated)
          state'    (merge state (zipmap activated updated))
          queue'    (pq/change-priority* queue (for [name activated]
                                                 [(get-in state [name :tn])
                                                  name
                                                  (get-in state' [name :tn])]))
          ;; TODO: Don't hardcode this.
          net-mail  (get mail-in [:root])
          _         (trace "net-mail: %s" net-mail)
          pkg'      (assoc pkg
                           :state state'
                           :queue queue')
          ;; ----- Update network structure -----
          act-execs (filter #(satisfies? IExecutive (get-in state' [% :model])) activated)
          _         (trace "act-execs: %s" (vec act-execs))
          pkg'      (reduce (fn [pkg name]
                              (binding [*path* (vec (butlast name))] ;; TODO: Consider using a parent field.
                                ;; Using old state!
                                (let [model (get-in state [name :model])
                                      state (get-in state [name :state])]
                                  (let [xs (network-structure-output model state)]
                                    (trace "network structure messages: %s" (count xs))
                                    (apply-network-structure-messages pkg xs t)))))
                            pkg'
                            act-execs)]
      [pkg' net-mail])))

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
