(ns pettomato.devs.dsde
  "A Clojure / FP implementation of the DSDE formalism described in:
  https://www.researchgate.net/profile/Fernando_Barros/publication/228767442_Representation_of_Dynamic_Structure_Discrete_Event_Models_A_Systems_Theory_Approach/links/00b7d526698beeebc0000000.pdf

  I've tried to mirror the mathematical treatment in the literature, without
  regard for efficiency.

  Output messages are stored with each simulator. In the transition, the set of
  influencers for each sim is checked to determine where to collect the input
  messages.

  Differences from the literature:

  The executive model never changes; just the state associated with it.

  The network structure contains models and routes, where models is a map from
  model names to models, and influencers are derived from routes.

  Messages are routed from sender to receiver, rather than backwards from each
  model to its influencers.

  The network output function does not return immediately if the network is not
  imminent, because the children still need to have their output values
  cleared. I think the paper is incorrect in this regard."
  (:require
   [pettomato.devs.models.network-structure :refer [network-name route-messages]]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.log :as log]))

;;------------------------------------------------------------------------------
;; Model Protocols

(defprotocol IModel
  "A protocol for an atomic model."
  (initial-total-state [this]                        "Returns [initial-state elapsed]. State can be anything; it is only used by the functions in this protocol.")
  (internal-update     [this state]                  "Returns new state.")
  (external-update     [this state elapsed messages] "Returns new state.")
  (confluent-update    [this state messages]         "Returns new state.")
  (partial-output      [this state]                  "Returns mail, which is a map from port name to a seq of values.")
  (time-advance        [this state]                  "Returns time until the next internal update."))

(defprotocol IExecutive
  "A protocol for an executive model."
  (network-structure [this state] "Returns a network structure, which is a map with fields: :models and :routes."))

(defprotocol INetwork
  "A protocol for a network model."
  (exec-name  [this] "Returns the name of the network executive.")
  (exec-model [this] "Returns the network executive model."))

;;------------------------------------------------------------------------------
;; Simulator Protocol

(defprotocol ISimulator
  "A protocol for a simulator."
  (start               [sim t]   "Initialization. Returns sim.")
  (output              [sim t]   "Compute output. Returns sim. (Output is stored locally.)")
  (transition          [sim x t] "Execute a state transition. Returns sim.")
  (time-of-last-update [sim]     "Time of the last sim update.")
  (time-of-next-update [sim]     "Scheduled time of the next sim internal update."))

;;------------------------------------------------------------------------------
;; Synchronizer

(defn simulate
  "Run a simulation from start-time to an optional end-time. If end-time is not
  provided, it defaults to infinity, and the simulation will run until the
  simulator returns infinity as its time-of-next-update, which means that it
  will never have another event, so we can quit."
  ([sim start-time]
   (simulate sim start-time infinity))
  ([sim start-time end-time]
   (loop [sim (start sim start-time)
          out []]
     (let [t (time-of-next-update sim)]
       (if (and (not= t infinity)
                (< t end-time))
         (let [sim' (output sim t)
               sim  (transition sim' {} t)]
           (recur sim (let [y (:y sim')]
                        (if (seq y)
                          (conj out [t y])
                          out))))
         out)))))

;;------------------------------------------------------------------------------
;; Simulator Implementations

(def ^:dynamic *sim-time* nil)
(def ^:dynamic *sim*      nil)
(def ^:dynamic *trace*    false)
(def ^:dynamic *indent*   0)

(defn pad-left
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
           (str "t=" (pad-left 5 \  (str *sim-time*))
                " sim=" (pad-left 10 \  (str *sim*))
                " | " (apply str (repeat *indent* \ ))
                (first args))
           (rest args))))

(declare model->sim)

(defrecord Simulator [id model state y tl tn]
  ISimulator
  (start [this t]
    (binding [*sim-time* t
              *sim*      id
              *indent*   (inc *indent*)]
      (trace "start")
      (let [[s e] (initial-total-state model)
            y     nil
            tl    (- t e)
            tn    (+ tl (time-advance model s))]
        (Simulator. id model s y tl tn))))
  (output [this t]
    ;; Note that the output is reset to nil if the model is not imminent. That
    ;; is not the same as doing nothing; it is ensuring that any previous output
    ;; is cleared, even if there is no new output.
    (binding [*sim-time* t
              *sim*      id
              *indent*   (inc *indent*)]
      (trace "output [tn=%s]" tn)
      #_
      (assoc this :y (if (= t tn)
                       (partial-output model state)
                       nil))
      (let [y (if (= t tn)
                (partial-output model state)
                nil)]
        (trace "y: %s" y)
        (Simulator. id model state y tl tn))))
  (transition [this x t]
    (binding [*sim-time* t
              *sim*      id
              *indent*   (inc *indent*)]
      (trace "transition: %s" x)
      (assert (<= tl t tn))
      (if (and (< t tn) (empty? x))
        this
        (let [state (if (empty? x)
                      (internal-update model state)
                      (if (= t tn)
                        (confluent-update model state x)
                        (external-update model state (- t tl) x)))
              tl    t
              tn    (+ tl (time-advance model state))]
          (Simulator. id model state y tl tn)))))
  (time-of-last-update [this] tl)
  (time-of-next-update [this] tn))

;; One flaw with this implementation is that it knows that the sims have :state
;; and :y fields, which is part of their implementation, not their protocol.
(defrecord Network [id model sims y tl tn]
  ISimulator
  (start [this t]
    (binding [*sim-time* t
              *sim*      id
              *indent*   (inc *indent*)]
      (trace "start")
      (let [exec-name        (exec-name model)
            exec-model       (exec-model model)
            exec-sim         (-> (model->sim exec-name exec-model)
                                 (start t))
            {:keys [models]} (network-structure exec-model (:state exec-sim))
            sims             (reduce-kv (fn [m k model]
                                          (assoc m k (-> (model->sim k model)
                                                         (start t))))
                                        {}
                                        models)
            sims             (assoc sims exec-name exec-sim)
            tl               (apply max (map time-of-last-update (vals sims)))
            tn               (apply min (map time-of-next-update (vals sims)))
            y                nil]
        (Network. id model sims y tl tn))))
  (output [this t]
    (binding [*sim-time* t
              *sim*      id
              *indent*   (inc *indent*)]
      (trace "output [tn=%s]" tn)
      ;;(trace "sims: %s" sims)
      ;; If this isn't imminent, its output is set to nil, but what about its
      ;; children???

      (let [sims             (zipmap (keys sims) (map #(output % t) (vals sims)))
            exec-name        (exec-name model)
            exec-model       (exec-model model)
            exec-sim         (get sims exec-name)
            {:keys [routes]} (network-structure exec-model (:state exec-sim))
            ;; This is doing more work than necessary; we only need to get the
            ;; output destined for the network.
            mail-out         (zipmap (keys sims) (map :y (vals sims)))
            mail-in          (route-messages routes mail-out)
            y                (get mail-in network-name)]
        (trace "network mail: %s" (or y {}))
        (Network. id model sims y tl tn))))
  (transition [this x t]
    (binding [*sim-time* t
              *sim*      id
              *indent*   (inc *indent*)]
      (trace "transition: %s" x)
      ;;(trace "sims: %s" sims)
      (assert (<= tl t tn))
      (if (and (< t tn) (empty? x))
        this
        (let [exec-name               (exec-name model)
              exec-model              (exec-model model)
              exec-sim                (get sims exec-name)
              {:keys [models routes]} (network-structure exec-model (:state exec-sim))
              mail-out                (-> (zipmap (keys sims) (map :y (vals sims)))
                                          (assoc network-name x))
              _                       (trace "mail-out: %s" (into {} (remove (comp empty? second) mail-out)))
              mail-in                 (route-messages routes mail-out)
              _                       (trace "mail-in: %s" (into {} (remove (comp empty? second) mail-in)))
              ;; In the literature, the exec is processed after the other models,
              ;; so that the network structure doesn't change before every model
              ;; is processed. In this implementation, the network structure is
              ;; stored, so all models can be updated in parallel.
              _                       (trace "START transition children ----------------------------")
              sims                    (reduce-kv (fn [m k sim]
                                                   (assoc m k (transition sim (get mail-in k) t)))
                                                 {}
                                                 sims)
              _                       (trace "END transition children ----------------------------")
              ;; store current structure
              models'                 models
              ;; get new structure
              exec-sim                (get sims exec-name)
              ;;_                       (trace "exec-sim: %s" [exec-sim])




              {:keys [models]}        (network-structure exec-model (:state exec-sim))
              ;; Flaw: This structure change implementation only compares model
              ;; names. It doesn't detect if the model changed.
              old-ks                  (set (keys models'))
              ;;_                       (trace "old-ks: %s" old-ks)
              new-ks                  (set (keys models))
              ;;_                       (trace "new-ks: %s" new-ks)
              sims                    (select-keys sims (conj new-ks exec-name))
              ;;_                       (trace "sims: %s" sims)


              new-models              (apply dissoc models exec-name old-ks)
              ;;_                       (trace "new-models: %s" sims)
              new-sims                (reduce-kv (fn [m k model]
                                                   (assoc m k (-> (model->sim k model)
                                                                  (start t))))
                                                 {}
                                                 new-models)
              sims                    (merge sims new-sims)
              ;;_                       (trace "new sims: %s" sims)
              ;;_                       (trace "merged sims: %s" sims)
              tl                      t
              tn                      (apply min (map time-of-next-update (vals sims)))]
          (Network. id model sims y tl tn)))))
  (time-of-last-update [this] tl)
  (time-of-next-update [this] tn))

(defn model->sim [id model]
  (condp satisfies? model
    IModel     (Simulator. id model nil nil nil nil)
    IExecutive (Simulator. id model nil nil nil nil)
    INetwork   (Network.   id model nil nil nil nil)))

;;------------------------------------------------------------------------------
;; Optimized Simulator Implementation

;; Proposal: Keep simulators for atomic models.
