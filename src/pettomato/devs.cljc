(ns pettomato.devs
  "This is my implementation, based loosely on the literature, and true to the
  logic, but functional and efficient."
  (:refer-clojure :exclude [run])
  (:require
   [clojure.set :refer [subset?]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.log :as log]))

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
#_
(defn trace-mail [label mail]
  (when *trace*
    (trace "%s" label)
    (doseq [[k vs] mail]
      (trace " %s -> %s" (vec (reverse k)) (vec vs)))))
#_
(defn trace-network-structure-message [name msg]
  (trace "%s" name)
  (case (first msg)
    :add-model  (let [[_ name model] msg] (trace (vec (butlast msg))))
    :rem-model  (let [[_ name]       msg] (trace msg))
    :connect    (let [[_ route]      msg] (trace msg))
    :disconnect (let [[_ route]      msg] (trace msg))))

;;------------------------------------------------------------------------------

(defmacro check-sync
  "A macro that takes an equality expression intended to represent a
  synchronization constraint, and expands into code that checks the contraint
  and throws a detailed exception if it is violated.

  Examples:
    (check-sync (< tl t tn))
    (check-sync (= t tn))"
  [expr]
  (let [[op & args] expr]
    `(when-not ~expr
       (let [m# (zipmap '~args (list ~@args))
             e# (str (list* '~op (list ~@args)))]
         (throw (ex-info (str "synchronization error: (not " e# ")")
                         m#))))))

;;------------------------------------------------------------------------------
;; Models

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
   :internal-update     internal-update-fn
   :external-update     external-update-fn
   :confluent-update    (or confluent-update-fn
                            (fn [s x] (external-update-fn (internal-update-fn s) 0 x)))
   :output              output-fn
   :time-advance        time-advance-fn})

(defn atomic-model? [model]
  (and (map? model)
       (subset? #{:initial-total-state
                  :internal-update
                  :external-update
                  :confluent-update
                  :output
                  :time-advance}
                (set (keys model)))))

(defn network-model [models routes]
  {:models models
   :routes routes})

(defn network-model? [model]
  (and (map? model)
       (subset? #{:models :routes}
                (set (keys model)))))

;;------------------------------------------------------------------------------
;; Simulator protocol

(defprotocol Simulator
  (initialize         [sim t]      "Initialize sim. Returns sim.")
  (collect-mail       [sim t]      "Returns [sim mail].") ;; Returns sim, b/c sim might need to store local mail.
  (transition         [sim mail t] "mail = p->vs. Returns sim.")
  (time-of-last-event [sim]        "Returns the time of the last sim update.")
  (time-of-next-event [sim]        "Returns the scheduled time of next sim internal update."))

;;------------------------------------------------------------------------------
;; Atomic Simulator

(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (initialize [sim t]
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance model) s))]
      (assoc sim :state s :tl tl :tn tn)))
  (collect-mail [sim t]
    (check-sync (= t tn))
    [sim ((:output model) state)])
  (transition [sim mail t]
    (check-sync (<= tl t tn))
    (let [state (if (empty? mail)
                  ((:internal-update model) state)
                  (if (= t tn)
                    ((:confluent-update model) state mail)
                    ((:external-update model) state (- t tl) mail)))
          tl    t
          tn    (+ tl ((:time-advance model) state))]
      (assoc sim :state state :tl tl :tn tn)))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(defn atomic-simulator [model]
  (map->AtomicSimulator {:model model}))

;;------------------------------------------------------------------------------
;; Network Simulator

(defn route-mail
  "Takes routes and outbound mail. Returns inbound mail.

  routes - {sk {sp {rk {rp fs}}}}

  outbound mail - {sk {sp vs}}

  inbound mail - {rk {rp vs}}"
  [routes mail]
  (reduce (fn [m [rk rp vs]]
            (update-in m [rk rp] into vs))
          {}
          (for [[sk sp->vs] mail
                [sp vs]     sp->vs
                [rk rp->fs] (get-in routes [sk sp])
                [rp fs]     rp->fs
                f           fs]
            [rk rp (map f vs)])))

(def merge-mail
  "Like clojure.core/merge, but specifically for mail data structures."
  (partial merge-with (partial merge-with into)))

(defn sort-mail
  "Groups inbound mail into three disjoint collections:
  [int-mail ext-mail ns-msgs]."
  [mail]
  (let [int-mail (dissoc mail :network)
        ext-mail (get mail :network)
        ns-msgs  (get ext-mail :structure)
        ext-mail (dissoc ext-mail :structure)]
    [int-mail ext-mail ns-msgs]))

(defn apply-transition
  "
  mail - p->vs
  "
  [network-sim k mail t]
  (let [sim  (get-in network-sim [:k->sim k])
        sim' (transition sim mail t)  ; recursive step
        tn   (time-of-next-event sim) ; Previously scheduled time; (<= t tn).
        tn'  (time-of-next-event sim')]
    (-> network-sim
        (assoc-in [:k->sim k] sim')
        (update :queue pq/change-priority tn k tn'))))

(defn apply-transitions [network-sim mail t]
  (reduce-kv (fn [network-sim k p->vs]
               (apply-transition network-sim k p->vs t))
             network-sim
             mail))

(defn add-model [network-sim k model t]
  (let [sim (init-sim model t)
        tn  (time-of-next-event sim)]
    (-> network-sim
        (update :k->sim assoc k sim)
        (update :queue pq/insert tn k))))

(defn rem-model [network-sim k]
  (let [sim (get-in network-sim [:k->sim k])
        tn  (time-of-next-event sim)]
    (-> network-sim
        (update :k->sim dissoc k)
        (update :queue pq/delete tn k))))

(defn connect [network-sim [sk sp rk rp f]]
  (-> network-sim
      (update-in [:routes sk sp rk rp] (fnil conj #{}) f)))

(defn disconnect [network-sim [sk sp rk rp f]]
  (-> network-sim
      (update-in [:routes sk sp rk rp] disj f)))

(defn apply-network-structure-changes [network-sim ns-msgs t]
  ;; Network structure messages are grouped and processed in a specific order.
  (let [ns-msgs    (group-by first ns-msgs)
        add-model  (fn [sim [_ k model]] (add-model  sim k model t))
        rem-model  (fn [sim [_ k]]       (rem-model  sim k))
        connect    (fn [sim [_ route]]   (connect    sim route))
        disconnect (fn [sim [_ route]]   (disconnect sim route))]
    (as-> network-sim network-sim
      (reduce disconnect network-sim (:disconnect ns-msgs))
      (reduce rem-model  network-sim (:rem-model  ns-msgs))
      (reduce add-model  network-sim (:add-model  ns-msgs))
      (reduce connect    network-sim (:connect    ns-msgs)))))

(declare init-sim)

(defrecord NetworkSimulator [model k->sim routes queue tl int-mail ns-msgs]
  Simulator
  (initialize [sim t]
    ;; Assuming initialize will only be called once.
    (let [sim (reduce-kv #(add-model %1 %2 %3 t) sim (:models model))
          sim (reduce connect sim (:routes model))
          tl  (apply min (map time-of-last-event (vals (:k->sim sim))))]
      (assoc sim :tl tl)))
  (collect-mail [sim t]
    (let [tn            (time-of-next-event sim)
          _             (check-sync (= t tn))
          imminent      (pq/peek queue)
          xs            (map #(collect-mail (k->sim %) t) imminent) ; recursive step
          k->sim'       (zipmap imminent (map first  xs))
          outbound-mail (zipmap imminent (map second xs))
          inbound-mail  (route-mail routes outbound-mail)
          [int-mail
           ext-mail
           ns-msgs]     (sort-mail inbound-mail)
          sim           (-> sim
                            (update :k->sim merge k->sim')
                            (assoc :int-mail int-mail
                                   :ns-msgs ns-msgs))]
      [sim ext-mail]))
  (transition [sim ext-mail t]
    (let [tn       (time-of-next-event sim)
          _        (check-sync (<= tl t tn))
          imminent (if (= t tn) (pq/peek queue) [])
          imm-mail (zipmap imminent (repeat {}))
          ext-mail (route-mail routes {:network ext-mail}) ; Assumption: There are no routes from :network to :network.
          mail     (merge-mail imm-mail int-mail ext-mail)]
      (-> sim
          (apply-transitions mail t)
          (apply-network-structure-changes ns-msgs t)
          (assoc :int-mail {})
          (assoc :ns-msgs []))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] (or (pq/peek-key queue) infinity)))

(defn network-simulator [model]
  (map->NetworkSimulator {:model    model
                          :queue    (pq/priority-queue)
                          :int-mail {}
                          :ns-msgs  []}))

;;------------------------------------------------------------------------------
;; Runner

(defn init-sim [model t]
  (let [sim-fn (cond
                 (atomic-model?  model) atomic-simulator
                 (network-model? model) network-simulator
                 :else                  (throw (ex-info "Unknown model type." {})))]
    (-> (sim-fn model)
        (initialize t))))

(defn run
  "Run a simulation based on the supplied model. Returns a seq of [timestamp mail].

  Options:

  start - Simulation start time (inclusive). Default: 0.

  end - Simulation end time (exclusive). Default: infinity.

  limit - Maximum number of iterations. Intended to prevent runaway
  simulations. Default: infinity.

  Note that the simulation will terminate before end time if the simulator's
  next internal update isn't before infinity."
  [model & {:keys [start end limit]
            :or   {start 0
                   end   infinity
                   limit infinity}}]
  (loop [sim (init-sim model start)
         out (transient [])
         i   0]
    (assert (< i limit))
    (let [t (time-of-next-event sim)]
      (if (< t end)
        (let [[sim out'] (binding [*sim-time* t] (collect-mail sim t))
              sim        (binding [*sim-time* t] (transition sim {} t))]
          (recur sim
                 (if (seq out')
                   (conj! out [t out'])
                   out)
                 (inc i)))
        (persistent! out)))))
