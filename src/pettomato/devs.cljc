(ns pettomato.devs
  (:refer-clojure :exclude [run])
  (:require
   [clojure.set :refer [subset?]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.log :as log]))

;;------------------------------------------------------------------------------
;; Simulation (dynamic) vars

(def ^{:dynamic true :private true} *path*
  "Bound to the path to the current model in the network hierarchy."
  [])

(def ^:dynamic *sim-time* nil)

;;------------------------------------------------------------------------------
;; Trace

(def ^:dynamic *trace* false)

(defn- pad-left
  "n - min string length of result
   c - char to add to the left
   s - string to add to"
  [n c s]
  (assert (char? c))
  (if (< (count s) n)
    (recur n c (str c s))
    s))

(defn format-time [t]
  (str "[" (pad-left 8 \  (str t)) "]"))

(defn format-path [path]
  (let [c (count path)]
    (if (pos? c)
      (let [i (* c 2)
            w (apply str (repeat (- i 2) \ ))
            a "|-"]
        (str w a (last path)))
      "")))

(defn trace [& args]
  (when *trace*
    (apply log/infof
           (str (format-time *sim-time*)
                " " (format-path *path*)
                (if (pos? (count *path*)) " " "")
                (first args))
           (rest args))))

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
  (collect-mail       [sim t]      "Returns [sim mail].") ; Returns sim, b/c sim might store local mail.
  (transition         [sim mail t] "mail = p->vs. Returns sim.")
  (time-of-last-event [sim]        "Returns the time of the last sim update.")
  (time-of-next-event [sim]        "Returns the scheduled time of next sim internal update."))

;;------------------------------------------------------------------------------
;; Atomic Simulator

(defrecord AtomicSimulator [model state tl tn]
  Simulator
  (initialize [sim t]
    (trace "--- initialize ---")
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance model) s))]
      (assoc sim :state s :tl tl :tn tn)))
  (collect-mail [sim t]
    (trace "--- collect-mail ---")
    (assert (= t tn) "synchronization error")
    [sim ((:output model) state)])
  (transition [sim mail t]
    (trace "--- transition ---")
    (assert (<= tl t tn) "synchronization error")
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

  routes        - {sk {sp {rk {rp fs}}}}
  outbound mail - {sk {sp vs}}
  inbound mail  - {rk {rp vs}}"
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
  [int-mail ext-mail net-msgs]."
  [mail]
  (let [int-mail (dissoc mail :network)
        ext-mail (get mail :network)
        net-msgs (get ext-mail :structure)
        ext-mail (dissoc ext-mail :structure)]
    [int-mail ext-mail net-msgs]))

(defn apply-transition
  "
  mail - p->vs
  "
  [network-sim k mail t]
  (let [sim  (get-in network-sim [:k->sim k])
        sim' (binding [*path* (conj *path* k)]
               (transition sim mail t))  ; recursive step
        tn   (time-of-next-event sim) ; Previously scheduled time; (<= t tn).
        tn'  (time-of-next-event sim')]
    (-> network-sim
        (assoc-in [:k->sim k] sim')
        (update :queue pq/change-priority tn k tn'))))

(defn apply-transitions [network-sim mail t]
  (reduce-kv #(apply-transition %1 %2 %3 t)
             network-sim
             mail))

(declare model->sim)

(defn add-model [network-sim k model t]
  (trace "add-model: %s" k)
  (let [sim (model->sim model)
        sim (binding [*path* (conj *path* k)]
              (initialize sim t))
        tn  (time-of-next-event sim)]
    (-> network-sim
        (update :k->sim assoc k sim)
        (update :queue pq/insert tn k))))

(defn rem-model [network-sim k]
  (trace "rem-model: %s" k)
  (let [sim (get-in network-sim [:k->sim k])
        tn  (time-of-next-event sim)]
    (-> network-sim
        (update :k->sim dissoc k)
        (update :queue pq/delete tn k))))

(defn connect [network-sim [sk sp rk rp f]]
  (trace "connect: %s" [sk sp rk rp f])
  (-> network-sim
      (update-in [:routes sk sp rk rp] (fnil conj #{}) f)))

(defn prune
  "Recursively removes empty leaves."
  [m ks]
  (if (seq ks)
    (let [[k & ks] ks
          v        (prune (get m k) ks)]
      (if (seq v)
        (assoc m k v)
        (dissoc m k)))
    m))

(defn disconnect [network-sim [sk sp rk rp f]]
  (trace "disconnect: %s" [sk sp rk rp f])
  (-> network-sim
      (update-in [:routes sk sp rk rp] disj f)
      (update :routes prune [sk sp rk rp])))

(defn apply-network-structure-changes [network-sim net-msgs t]
  ;; Network structure messages are grouped and processed in a specific order.
  ;; Himmelspach, Jan, and Adelinde M. Uhrmacher. "Processing dynamic PDEVS models."
  ;; The IEEE Computer Society's 12th Annual International Symposium on Modeling, Analysis, and Simulation of Computer and Telecommunications Systems, 2004.
  ;; Section 3.2
  ;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.3385&rep=rep1&type=pdf
  (let [net-msgs   (group-by first net-msgs)
        add-model  (fn [sim [_ k model]] (add-model  sim k model t))
        rem-model  (fn [sim [_ k]]       (rem-model  sim k))
        connect    (fn [sim [_ route]]   (connect    sim route))
        disconnect (fn [sim [_ route]]   (disconnect sim route))]
    (as-> network-sim network-sim
      (reduce disconnect network-sim (:disconnect net-msgs))
      (reduce rem-model  network-sim (:rem-model  net-msgs))
      (reduce add-model  network-sim (:add-model  net-msgs))
      (reduce connect    network-sim (:connect    net-msgs)))))

(defrecord NetworkSimulator [model k->sim routes queue int-mail net-msgs]
  Simulator
  (initialize [sim t]
    (trace "--- initialize ---")
    ;; Assuming initialize will only be called once.
    (reduce connect
            (reduce-kv #(add-model %1 %2 %3 t) sim (:models model))
            (:routes model)))
  (collect-mail [sim t]
    (trace "--- collect-mail ---")
    (assert (= t (time-of-next-event sim)) "synchronization error")
    (let [imminent      (pq/peek queue)
          _             (trace "imminent: %s" imminent)
          xs            (map (fn [k]
                               (binding [*path* (conj *path* k)]
                                 (collect-mail (k->sim k) t))) ; recursive step
                             imminent)
          k->sim'       (zipmap imminent (map first  xs))
          outbound-mail (zipmap imminent (map second xs))
          _             (trace "outbound-mail: %s" outbound-mail)
          inbound-mail  (route-mail routes outbound-mail)
          _             (trace " inbound-mail: %s" inbound-mail)
          [int-mail
           ext-mail
           net-msgs]    (sort-mail inbound-mail)
          sim           (-> sim
                            (update :k->sim merge k->sim')
                            (assoc :int-mail int-mail
                                   :net-msgs net-msgs))]
      [sim ext-mail]))
  (transition [sim ext-mail t]
    (trace "--- transition ---")
    (assert (<= (time-of-last-event sim) t (time-of-next-event sim)) "synchronization error")
    (let [tn       (time-of-next-event sim)
          imminent (if (= t tn) (pq/peek queue) [])
          _        (trace "imminent: %s" imminent)
          imm-mail (zipmap imminent (repeat {}))
          ext-mail (route-mail routes {:network ext-mail}) ; Assumption: There are no routes from :network to :network.
          _        (trace "ext-mail: %s" ext-mail)
          mail     (merge-mail imm-mail int-mail ext-mail)
          _        (trace "mail: %s" mail)]
      (-> sim
          (apply-transitions mail t)
          (apply-network-structure-changes net-msgs t)
          (assoc :int-mail {})
          (assoc :net-msgs []))))
  (time-of-last-event [sim] (apply max (map time-of-last-event (vals k->sim))))
  (time-of-next-event [sim] (or (pq/peek-key queue) infinity)))

(defn network-simulator [model]
  (map->NetworkSimulator {:model    model
                          :queue    (pq/priority-queue)
                          :int-mail {}
                          :net-msgs []}))

;;------------------------------------------------------------------------------
;; Runner

(defn model->sim [model]
  (cond
    (atomic-model?  model) (atomic-simulator  model)
    (network-model? model) (network-simulator model)
    :else                  (throw (ex-info "Unknown model type." {}))))

;; aka root coordinator
(defn run
  "Run a simulation \"as fast as possible\".

  Returns a seq of [timestamp mail].

  Options:

  start - Simulation start time (inclusive). Default: 0.

  end - Simulation end time (exclusive). Default: infinity.

  limit - Maximum number of iterations. Intended to prevent runaway
  simulations. Default: infinity.

  Note that the simulation will terminate before end time if the simulator's
  next internal update isn't before infinity."
  [sim & {:keys [start end limit]
          :or   {start 0
                 end   infinity
                 limit infinity}
          :as   options}]
  (trace "run {:start %s :end %s :limit %s}" start end limit)
  (loop [sim (binding [*sim-time* start] (initialize sim start))
         out (transient [])
         i   0]
    (assert (< i limit) (str "limit reached: " i))
    (let [t (time-of-next-event sim)]
      (binding [*sim-time* t] (trace "[ step %s ] --------------------------------------------------" i))
      (if (< t end)
        (let [[sim out'] (binding [*sim-time* t] (collect-mail sim t))
              sim        (binding [*sim-time* t] (transition sim {} t))]
          (recur sim
                 (if (seq out')
                   (conj! out [t out'])
                   out)
                 (inc i)))
        (do (trace "END {:start %s :end %s :limit %s}" start end limit)
            (persistent! out))))))
#_
(defn lazy-afap-root-coordinator [sim start-time end-time]
  (letfn [(step [sim]
            (let [t (time-of-next-event sim)]
              (if (< end-time t)
                nil
                (let [[sim' out'] (receive-*-message sim t)
                      sim'        (receive-x-message sim' {} t)]
                  (if (seq out')
                    (cons [t out'] (lazy-seq (step sim')))
                    (lazy-seq (step sim')))))))]
    (lazy-seq (step (receive-i-message sim start-time)))))
