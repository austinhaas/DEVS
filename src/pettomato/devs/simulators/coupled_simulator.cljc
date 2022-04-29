(ns pettomato.devs.simulators.coupled-simulator
  "A simulator for coupled models. Aka, coordinator."
  (:require
   [clojure.set :as set]
   [pettomato.devs.lib.coll :refer [prune]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :as mail]
   [pettomato.devs.lib.priority-queue :as pq]
   [pettomato.devs.simulator :refer [Simulator initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.coupled-model :refer [models routes coupled-model?]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]))

(declare coupled-simulator)

(defn find-simulator
  "A function that takes two args: an id and a model, and returns an
  appropriate simulator for that model."
  [id model]
  (cond
    (atomic-model?  model) atomic-simulator
    (coupled-model? model) coupled-simulator
    :else                  (throw (ex-info "Unknown model type." {}))))

(defn- add-model [parent-sim id [model elapsed] t]
  (log/tracef "add-model: %s" id)
  (assert (not (contains? (:id->sim parent-sim) id))
          (str "parent-sim already contains a model with id: " id))
  (let [simulator (find-simulator id model)
        sim       (-> model (simulator :elapsed elapsed) (initialize t))
        tn        (time-of-next-event sim)]
    (-> parent-sim
        (update :id->sim assoc id sim)
        (update :queue pq/insert tn id))))

(defn- rem-model [parent-sim id]
  (log/tracef "rem-model: %s" id)
  (let [sim (get-in parent-sim [:id->sim id])
        _   (assert sim (str "parent-sim does not contain a model with id: " id))
        tn  (time-of-next-event sim)]
    (-> parent-sim
        (update :id->sim dissoc id)
        (update :queue pq/delete tn id))))

(defn- connect
  "Add a route to the routing table.

  f is optional; defaults to the identity function."
  [parent-sim [sk sp rk rp f]]
  (log/tracef "connect: %s" [sk sp rk rp f])
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
  (log/tracef "disconnect: %s" [sk sp rk rp f])
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
        sim' (transition sim mail t)  ; Recursive step.
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

(defrecord CoupledSimulator [model id->sim local-routes network-input-routes network-output-routes queue tl tn]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    ;; Assuming initialize will only be called once.
    (as-> sim sim
      (reduce-kv #(add-model %1 %2 %3 t) sim (models model))                       ; Add models.
      (reduce connect sim (routes model))                                          ; Add routes.
      (assoc sim :tl (apply h/max (map time-of-last-event (vals (:id->sim sim))))) ; Cache tl.
      (assoc sim :tn (or (pq/peek-key (:queue sim)) h/infinity))))                 ; Cache tn.
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (assert (h/= t tn) (str "synchronization error: (not (= " t " " tn "))"))
    (->> (set/intersection (pq/peek queue) (set (keys network-output-routes))) ; The set of model ids that have network-output-routes could be cached.
         (select-keys id->sim)
         (reduce-kv (fn [m id sim] (assoc m id (collect-mail sim t))) {})
         (mail/route-mail network-output-routes)
         :network))
  (transition [sim mail t]
    (log/tracef "--- transition --- %s" t)
    (assert (h/<= tl t tn) (str "synchronization error: (not (<= " tl " " t " " tn "))"))
    (when (and (h/< t tn) (empty? mail))
      (throw (ex-info "Illegal state for transition; sim is not imminent nor receiving mail."
                      {:tl         tl
                       :t          t
                       :tn         tn
                       :mail-count (count mail)})))
    (let [imminent     (if (h/= t tn) (pq/peek queue) [])
          imm-mail     (zipmap imminent (repeat {})) ; Transitions are "mail-driven"; imminent sims are primed with an empty bag.
          ;; There could be redundancy here, if a component routes
          ;; output to the network (in collect-mail above) and also
          ;; locally (here).
          local-mail   (->> (set/intersection imminent (set (keys local-routes))) ; The set of model ids that have local-routes could be cached.
                            (select-keys id->sim)
                            (reduce-kv (fn [m id sim] (assoc m id (collect-mail sim t))) {})
                            (mail/route-mail local-routes))
          network-mail (mail/route-mail network-input-routes {:network mail})
          all-mail     (mail/merge-mail imm-mail local-mail network-mail)
          sim          (apply-transitions sim t all-mail)
          tn           (or (pq/peek-key (:queue sim))
                           h/infinity)]
      (assoc sim :tl t :tn tn)))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(defn coupled-simulator
  "Wrap a coupled model in a CoupledSimulator.

  Args:
    model - A coupled model.

  Returns:
    A CoupledSimulator."
  [model & {:keys [elapsed]
            :or   {elapsed h/zero}}]
  (log/infof "Ignoring elapsed input to coupled-simulator.")
  (map->CoupledSimulator {:model                 model
                          :local-routes          {}
                          :network-input-routes  {}
                          :network-output-routes {}
                          :queue                 (pq/priority-queue h/comparator)}))
