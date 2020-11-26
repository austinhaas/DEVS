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

(defprotocol Simulator
  (initialize         [sim t]      "Initialize sim. Returns sim.")
  (collect-mail       [sim t]      "Returns [sim mail].")
  (struct-changes     [sim t]      "Returns [sim changes].")
  (transition         [sim mail t] "mail = port->vs. Returns sim.")  ;; Consider returning tn.
  (time-of-last-event [sim]        "Time of the last sim update.")
  (time-of-next-event [sim]        "Scheduled time of next sim internal update."))

(defrecord AtomicSimulator [model
                            state
                            tl
                            tn]
  Simulator
  (initialize [this t]
    (let [[s e] (:initial-total-state model)
          tl    (- t e)
          tn    (+ tl ((:time-advance model) s))]
      (AtomicSimulator. model s tl tn)))
  (collect-mail [this t]
    (when-not (= t tn)
      (throw (ex-info (str "synchronization error" " (not (= " t " " tn "))")
                      {:t t :tn tn :tl tl})))
    [this ((:output model) state)])
  (struct-changes [this t]
    (when-not (= t tn)
      (throw (ex-info (str "synchronization error" " (not (= " t " " tn "))")
                      {:t t :tn tn :tl tl})))
    ((:network-structure model) state))
  (transition [this mail t]
    (when-not (<= tl t tn)
      (throw (ex-info (str "synchronization error" " (not (<= " tl " " t " " tn "))")
                      {:t t :tn tn :tl tl})))
    (let [state (if (empty? mail)
                  ((:internal-update model) state)
                  (if (= t tn)
                    ((:confluent-update model) state mail)
                    ((:external-update model) state (- t tl) mail)))
          tl    t
          tn    (+ tl ((:time-advance model) state))]
      (AtomicSimulator. model state tl tn)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn))

(defn atomic-simulator [model]
  (AtomicSimulator. model nil nil nil))

(defn route [routes sp->vs]
  (reduce (fn [m [rk rp vs]]
            (update-in m [rk rp] into vs))
          {}
          (for [[sp vs]     sp->vs
                [rk rp->fs] (get routes sp)
                [rp fs]     rp->fs
                f           fs]
            [rk rp (map f vs)])))

(def merge-mail (partial merge-with (partial merge-with into)))

(declare network-simulator)

(defrecord NetworkSimulator [model
                             k->sim
                             routes
                             queue
                             tl
                             int-mail]
  Simulator
  (initialize [this t]
    (let [k->sim (reduce-kv (fn [m k model']
                              (let [sim  (cond
                                           (atomic-model?  model') (atomic-simulator  model')
                                           (network-model? model') (network-simulator model')
                                           :else                   (throw (ex-info "Unknown model type." {:k k})))
                                    sim' (initialize sim t)]
                                (assoc m k sim')))
                            {}
                            (:models model))
          routes (reduce (fn [m [sk sp rk rp f]]
                           (update-in m [sk sp rk rp] (fnil conj #{}) f))
                         {}
                         (:routes model))
          queue  (reduce-kv (fn [pq k sim]
                              (pq/insert pq (time-of-next-event sim) k))
                            (pq/priority-queue)
                            k->sim)
          tl     (apply min (map time-of-last-event (vals k->sim)))]
      (NetworkSimulator. model k->sim routes queue tl {})))
  (collect-mail [this t]
    (let [tn (time-of-next-event this)]
      (when-not (= t tn)
        (throw (ex-info (str "synchronization error" " (not (= " t " " tn "))")
                        {:t t :tn tn :tl tl}))))
    ;; It is safe to assume that there is at least one imminent sim.
    ;; `loop` was chosen to simplify updating multiple variables.
    (let [imminent (pq/peek queue)]
      (loop [[k & ks] imminent
             k->sim   k->sim
             mail     {}]
        (let [sim          (k->sim k)
              [sim' mail'] (collect-mail sim t) ;; recursive step
              k->sim       (assoc k->sim k sim')
              mail         (merge-mail mail (route (get routes k) mail'))]
          (if (seq ks)
            (recur ks k->sim mail)
            (let [int-mail (dissoc mail :network) ;; k->p->vs
                  ext-mail (get mail :network)]     ;; p->vs
              [(NetworkSimulator. model k->sim routes queue tl int-mail)
               ext-mail]))))))
  (struct-changes [this t]
    (let [tn (time-of-next-event this)]
      (when-not (= t tn)
        (throw (ex-info (str "synchronization error" " (not (= " t " " tn "))")
                        {:t t :tn tn :tl tl}))))
    ;; TODO: Fix this
    [])
  (transition [this mail t]
    (let [tn (time-of-next-event this)]
      (when-not (<= tl t tn)
        (throw (ex-info (str "synchronization error" " (not (<= " tl " " t " " tn "))")
                        {:t t :tn tn :tl tl}))))
    (let [ext-mail       (route (get routes :network) mail) ;; Assumption: There are no routes from :network to :network.
          imminent       (if (= t (time-of-next-event this))
                           (pq/peek queue)
                           [])
          k->mail        (merge-mail (zipmap imminent (repeat {}))
                                     int-mail
                                     ext-mail)
          struct-changes (mapcat (comp #(struct-changes % t) k->sim) imminent)]
      ;; It is safe to assume that there is at least one activated sim.
      ;; `loop` was chosen to simplify updating multiple variables.
      (let [sim (loop [[[k mail] & k->mail] (seq k->mail)
                       k->sim               k->sim
                       queue                queue]
                  (let [sim    (k->sim k)
                        sim'   (transition sim mail t) ;; recursive step
                        k->sim (assoc k->sim k sim')
                        tn     (time-of-next-event sim)
                        tn'    (time-of-next-event sim')
                        queue  (pq/change-priority queue tn k tn')]
                    (if (seq k->mail)
                      (recur k->mail k->sim queue)
                      #_
                      (NetworkSimulator. model k->sim routes queue t {})

                      (loop [[msg & msgs] struct-changes
                             k->sim       k->sim
                             routes       routes
                             queue        queue]
                        (if msg
                          (case (first msg)
                            :add-model  (let [[_ k model] msg]
                                          (let [sim  (cond
                                                       (atomic-model?  model) (atomic-simulator  model)
                                                       (network-model? model) (network-simulator model)
                                                       :else                  (throw (ex-info "Unknown model type." {:k k})))
                                                sim' (initialize sim t)]
                                            (recur msgs
                                                   (assoc k->sim k sim')
                                                   routes
                                                   (pq/insert queue (time-of-next-event sim') k))))
                            :rem-model  (let [[_ k] msg]
                                          (let [sim (k->sim k)]
                                            (recur msgs
                                                   (dissoc k->sim k)
                                                   ;; TODO: Also remove routes where this is receiving.
                                                   (dissoc routes k)
                                                   (pq/delete queue (time-of-next-event sim) k))))
                            :connect    (let [[_ [sk sp rk rp f]] msg]
                                          (recur msgs
                                                 k->sim
                                                 (update-in routes [sk sp rk rp] (fnil conj #{}) f)
                                                 queue))
                            :disconnect (let [[_ [sk sp rk rp f]] msg]
                                          (recur msgs
                                                 k->sim
                                                 (update-in routes [sk sp rk rp] disj f)
                                                 queue)))
                          (NetworkSimulator. model k->sim routes queue t {})))
                      )))]
        sim
        )))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] (or (pq/peek-key queue) infinity)))

(defn network-simulator [model]
  (NetworkSimulator. model nil nil nil nil nil))

(defn run
  "Run a simulation from start-time (inclusive) to end-time (exclusive). If
  end-time is not provided, it defaults to infinity, and the simulation will run
  until the simulator returns infinity as its time-of-next-update, which means
  that it will never have another event, so it is safe to quit.

  Returns a seq of [timestamp mail]."
  ([model start-time]
   (run model start-time infinity))
  ([model start-time end-time]
   (loop [sim  (-> (cond
                     (atomic-model?  model) (atomic-simulator  model)
                     (network-model? model) (network-simulator model)
                     :else                  (throw (ex-info "Unknown model type." {})))
                   (initialize start-time))
          out  []
          i    0]
     (assert (< i 1000))
     (let [t (time-of-next-event sim)]
       (if (< t end-time)
         (let [[sim out'] (binding [*sim-time* t] (collect-mail sim t))
               sim        (binding [*sim-time* t] (transition sim {} t))]
           (recur sim (if (seq out')
                        (conj out [t out'])
                        out) (inc i)))
         out)))))
