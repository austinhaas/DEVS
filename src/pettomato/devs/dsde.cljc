(ns pettomato.devs.dsde
  "
  https://www.researchgate.net/profile/Fernando_Barros/publication/228767442_Representation_of_Dynamic_Structure_Discrete_Event_Models_A_Systems_Theory_Approach/links/00b7d526698beeebc0000000.pdf
  "
  (:require
   [clojure.set :refer [difference]]
   [pettomato.devs.models.network-structure :refer [network-name factor-routes route-messages]]
   [pettomato.devs.util :refer [infinity dissoc-in]]))

;; This implementation resembles the mathematical descriptions in the
;; literature; it is not efficient.

;; Output messages are stored with each simulator. In the transition, the set of
;; influencers for each sim is checked to determine where to collect the input
;; messages.

(defprotocol IModel
  (initial-total-state [model]                        "Returns [initial-state elapsed].")
  (internal-update     [model state]                  "Returns new state.")
  (external-update     [model state elapsed messages] "Returns new state.")
  (confluent-update    [model state messages]         "Returns new state.")
  (partial-output      [model state]                  "Returns mail.")
  (time-advance        [model state]                  "Returns time until next internal update."))

(defprotocol IExecutive
  (network-structure [model state] "Returns a network structure."))

(defprotocol INetwork
  (exec-name  [model] "The name of the network executive.")
  (exec-model [model] "The model for the network executive."))

(defprotocol ISimulator
  (start      [sim t]   "Initialization. Returns sim.")
  (output     [sim t]   "Compute output. Returns sim.")
  (transition [sim x t] "Execute a state transition. Returns sim.")
  (tl         [sim]     "Time of the last sim update.")
  (tn         [sim]     "Scheduled time of next sim internal update."))

(defrecord Simulator [id model state y tl tn]
  ISimulator
  (start [this t]
    (let [[s e] (initial-total-state model)
          y     nil
          tl    (- t e)
          tn    (+ tl (time-advance model s))]
      (Simulator. id model s y tl tn)))
  (output [this t]
    (let [y (if (= t tn)
              (partial-output model state)
              nil)]
      (Simulator. id model state y tl tn)))
  (transition [this x t]
    (assert (<= tl t tn))
    (if (and (< t tn) (empty? x))
      this
      (let [state (cond
                    (and (empty? x)
                         (= t tn))     (internal-update model state)
                    (and (seq x)
                         (= t tn))     (confluent-update model state x)
                    (and (seq x)
                         (<= tl t tn)) (external-update model state (- t tl) x)
                    :else              (throw (ex-info "Can't determine which state transition function to call."
                                                       {:x x :tl tl :t t :tn tn})))
            tl    t
            tn    (+ tl (time-advance model state))]
       (Simulator. id model state y tl tn))))
  (tl [this] tl)
  (tn [this] tn))

;; model = {:exec-name :exec-model}

;; exec-model = atomic + :network-fn

;; network-structure = {:models :routes}, where models replaces D and {M}.

;; Consider not using a map for sims. Use a list, add/rem exec to/from front
;; when necessary.

(defn get-mail [routes sims k]
  (reduce (fn [m [out-model out-port in-model in-port f]]
            (if (= k in-model)
              (if-let [vs (seq (get-in sims [out-model :y out-port]))]
                (update m in-port into vs)
                m)
              m))
          {}
          routes))

;; In this version the exec model never changes, just the state.

(defrecord Network [id model sims y tl tn]
  ISimulator
  (start [this t]
    (let [exec-name        (exec-name model)
          exec-model       (exec-model model)
          exec-sim         (-> (Simulator. exec-name exec-model nil nil nil nil)
                               (start t))
          exec-state       (:state exec-sim)
          {:keys [models]} (network-structure exec-model exec-state)
          sims             (zipmap (keys models)
                                   (map #(start % t) (vals models)))
          sims             (assoc sims exec-name exec-sim)
          tl               (apply max (map tl (vals sims)))
          tn               (apply min (map tn (vals sims)))
          y                nil]
      (Network. id model sims y tl tn)))
  (output [this t]
    (if (= t tn)
      (let [sims             (zipmap (keys sims)
                                     (map #(output % t) (vals sims)))
            exec-name        (exec-name model)
            exec-model       (exec-model model)
            exec-sim         (get sims exec-name)
            exec-state       (:state exec-sim)
            {:keys [routes]} (network-structure exec-model exec-state)
            routes           (factor-routes routes)
            mail-out         (zipmap (keys sims) (map :y (vals sims)))
            mail-in          (route-messages routes mail-out)
            y                (get mail-in network-name)]
        (Network. id model sims y tl tn))
      (Network. id model sims nil tl tn)))
  (transition [this x t]
      (assert (<= tl t tn))
      (if (and (< t tn) (empty? x))
        this
        (let [exec-name               (exec-name model)
              exec-model              (exec-model model)
              exec-sim                (get sims exec-name)
              exec-state              (:state exec-sim)
              {:keys [models routes]} (network-structure exec-model exec-state)
              models'                 models
              routes                  (factor-routes routes)
              mail-out                (assoc (zipmap (keys sims) (map :y (vals sims)))
                                             :network-name x)
              mail-in                 (route-messages routes mail-out)
              ;; No need to handle the exec separately, the current network
              ;; structure has been stored.
              sims                    (reduce-kv (fn [m k sim]
                                                   (transition sim t (get mail-in k)))
                                                 {}
                                                 sims)
              exec-sim                (get sims exec-name)
              exec-state              (:state exec-sim)
              {:keys [models]}        (network-structure exec-model exec-state)
              old-ks                  (set (keys models'))
              new-ks                  (set (keys models))
              sims                    (select-keys sims new-ks)
              new-models              (dissoc models old-ks)
              new-sims                (zipmap (keys new-models)
                                              (map #(start % t) (vals new-models)))
              sims                    (merge sims new-sims)
              tl                      t
              tn                      (apply min (map tn (vals sims)))]
          (Network. id model sims y tl tn))))
  (tl [this] tl)
  (tn [this] tn))

(defn simulate [sim t]
  (let [sim (start sim t)
        out []]
    (let [t (tn sim)]
      (if (not= t infinity)
        (let [out' (output sim t)
              sim  (transition sim {} t)]
          (recur sim (if (seq out')
                       (conj out [t out'])
                       out)))
        out))))

(comment

  (let [model ()]

      (simulate))




  )
