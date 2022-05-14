(ns pettomato.devs.simulators.network-simulator
  "A simulator for dynamic structure network models."
  (:require
   [clojure.set :as set]
   [pettomato.devs.lib.coll :refer [prune]]
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :as mail]
   [pettomato.devs.lib.priority-queue :as pq]
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.executive-model :refer [executive-model?]]
   [pettomato.devs.models.network-model :refer [network-model?]]
   [pettomato.devs.simulator :refer [Simulator initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.executive-simulator :refer [executive-simulator get-structure-changes]]))

(declare network-simulator)

(defn find-simulator
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
  (log/tracef "add-model: %s" [id elapsed])
  (ex-assert (not (contains? (:id->sim parent-sim) id))
             "duplicate id"
             {:id id})
  (let [simulator (find-simulator id model)
        sim       (-> model (simulator :elapsed elapsed) (initialize t))
        tl        (time-of-last-event sim)
        tn        (time-of-next-event sim)]
    (ex-assert (h/< t tn))
    (as-> parent-sim s
      (update s :id->sim assoc id sim)
      (update s :queue pq/insert tn id)
      (assoc s :tl (h/max (:tl parent-sim) tl))
      (assoc s :tn (h/min (:tn parent-sim) tn)))))

(defn- rem-model [parent-sim id]
  (log/tracef "rem-model: %s" id)
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
    (log/trace "--- initialize ---")
    (let [sim        (assoc sim
                            :id->sim               {}
                            :local-routes          {}
                            :network-input-routes  {}
                            :network-output-routes {}
                            :queue                 (pq/priority-queue h/comparator)
                            :tl                    (h/*R -1 0 0)
                            :tn                    (h/*R  1 0 0))
          exec-id    (:executive-id model)
          exec-model (:executive-model model)
          t'         (h/- t initial-elapsed)]
      (as-> sim sim
        (add-model sim exec-id exec-model t')
        (reduce-kv #(add-model %1 %2 %3 t') sim (:models model))
        (reduce connect sim (:routes model)))))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (ex-assert (h/= t tn)
               "synchronization error"
               {:t t :tn tn})
    (->> (set/intersection (pq/peek queue) (set (keys network-output-routes))) ; The set of model ids that have network-output-routes could be cached.
         (select-keys id->sim)
         (reduce-kv (fn [m id sim] (assoc m id (collect-mail sim t))) {})
         (mail/route-mail network-output-routes)
         :network))
  (transition [sim mail t]
    (log/trace "--- transition ---")
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
                              (get-structure-changes (get (:id->sim sim) (:executive-id model)))
                              [])]
      #_
      (ex-assert (h/< tl tn)
                 "tn must be greater than tl."
                 {:tl tl :tn tn})
      (as-> sim s
        (apply-transitions s t all-mail)
        (apply-network-structure-changes s t structure-changes)
        (assoc s
               :tl t
               :tn (pq/peek-key (:queue s))))))
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
