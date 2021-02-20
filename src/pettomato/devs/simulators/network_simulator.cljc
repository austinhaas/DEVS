(ns pettomato.devs.simulators.network-simulator
  (:require
   [pettomato.devs.lib.coll :refer [prune]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :refer [merge-mail route-mail sort-mail]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.lib.priority-queue :as pq]
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.network-model :refer [network-model?]]
   [pettomato.devs.simulator :refer [Simulator initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.vars :refer [*path*]]))

(defn apply-transition
  "Invoke a transition for a single simulator.

  network-sim - The network simulator.

  k - The name of the component simulator.

  mail - The local mail (p->vs) for the component simulator.

  t - The current sim time."
  [network-sim k mail t]
  (let [sim  (get-in network-sim [:k->sim k])
        sim' (binding [*path* (conj *path* k)]
               (transition sim mail t))  ; recursive step
        tn   (time-of-next-event sim) ; Previously scheduled time; (<= t tn).
        tn'  (time-of-next-event sim')]
    (-> network-sim
        (assoc-in [:k->sim k] sim')
        (update :queue pq/change-priority tn k tn'))))

(defn- apply-transitions
  "Invoke a transition across all component simulators.

  network-sim - The network simulator.

  mail - Inbound mail for this simulator (k->p->vs).

  t - The current sim time."
  [network-sim mail t]
  ;; Note that this could be made to run in parallel.
  (reduce-kv #(apply-transition %1 %2 %3 t)
             network-sim
             mail))

(defn- add-model [model->sim network-sim k model t]
  (log/tracef "add-model: %s" k)
  (let [sim (model->sim model)
        sim (binding [*path* (conj *path* k)]
              (initialize sim t))
        tn  (time-of-next-event sim)]
    (-> network-sim
        (update :k->sim assoc k sim)
        (update :queue pq/insert tn k))))

(defn- rem-model [network-sim k]
  (log/tracef "rem-model: %s" k)
  (let [sim (get-in network-sim [:k->sim k])
        tn  (time-of-next-event sim)]
    (-> network-sim
        (update :k->sim dissoc k)
        (update :queue pq/delete tn k))))

(defn- connect [network-sim [sk sp rk rp f]]
  (log/tracef "connect: %s" [sk sp rk rp f])
  (-> network-sim
      (update-in [:routes sk sp rk rp] (fnil conj #{}) f)))

(defn- disconnect [network-sim [sk sp rk rp f]]
  (log/tracef "disconnect: %s" [sk sp rk rp f])
  (-> network-sim
      (update-in [:routes sk sp rk rp] disj f)
      (update :routes prune [sk sp rk rp])))

(defn- apply-network-structure-changes [model->sim network-sim net-msgs t]
  ;; Network structure messages are grouped and processed in a specific order.
  ;; Himmelspach, Jan, and Adelinde M. Uhrmacher. "Processing dynamic PDEVS models."
  ;; The IEEE Computer Society's 12th Annual International Symposium on Modeling, Analysis, and Simulation of Computer and Telecommunications Systems, 2004.
  ;; Section 3.2
  ;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.3385&rep=rep1&type=pdf
  (let [net-msgs   (group-by first net-msgs)
        add-model  (fn [sim [_ k model]] (add-model  model->sim sim k model t))
        rem-model  (fn [sim [_ k]]       (rem-model  sim k))
        connect    (fn [sim [_ route]]   (connect    sim route))
        disconnect (fn [sim [_ route]]   (disconnect sim route))]
    (as-> network-sim network-sim
      (reduce disconnect network-sim (:disconnect net-msgs))
      (reduce rem-model  network-sim (:rem-model  net-msgs))
      (reduce add-model  network-sim (:add-model  net-msgs))
      (reduce connect    network-sim (:connect    net-msgs)))))

(defrecord NetworkSimulator [model k->sim routes queue int-mail net-msgs model->sim tl]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    ;; Assuming initialize will only be called once.
    (let [sim (as-> sim sim
                ;; Add models.
                (reduce-kv #(add-model model->sim %1 %2 %3 t) sim (:models model))
                ;; Add routes.
                (reduce connect sim (:routes model)))]
      (assoc sim :tl (reduce max (map time-of-last-event (vals (:k->sim sim)))))))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (assert (= t (time-of-next-event sim))
            (str "synchronization error: (not (= " t " " (time-of-next-event sim) ")"))
    (let [imminent      (pq/peek queue)
          _             (log/tracef "imminent: %s" imminent)
          ;; Note that this could be made to run in parallel.
          sim-and-mail  (map (fn [k]
                               (binding [*path* (conj *path* k)]
                                 (collect-mail (k->sim k) t))) ; recursive step
                             imminent)
          k->sim'       (zipmap imminent (map first  sim-and-mail))
          outbound-mail (zipmap imminent (map second sim-and-mail))
          _             (log/tracef "outbound-mail: %s" outbound-mail)
          inbound-mail  (route-mail routes outbound-mail)
          _             (log/tracef " inbound-mail: %s" inbound-mail)
          [int-mail
           ext-mail
           net-msgs]    (sort-mail inbound-mail)
          sim           (-> sim
                            (update :k->sim merge k->sim')
                            (assoc :int-mail int-mail
                                   :net-msgs net-msgs))]
      [sim ext-mail]))
  (transition [sim ext-mail t]
    (log/trace "--- transition ---")
    (assert (<= (time-of-last-event sim) t (time-of-next-event sim))
            (str "synchronization error: (not (<= " (time-of-last-event sim) " " t " " (time-of-next-event sim) ")"))
    (let [tn       (time-of-next-event sim)
          imminent (if (= t tn) (pq/peek queue) [])
          _        (log/tracef "imminent: %s" imminent)
          imm-mail (zipmap imminent (repeat {}))
          ext-mail (route-mail routes {:network ext-mail}) ; Assumption: There are no routes from :network to :network.
          _        (log/tracef "ext-mail: %s" ext-mail)
          mail     (merge-mail imm-mail int-mail ext-mail)
          _        (log/tracef "mail: %s" mail)]
      (-> sim
          (apply-transitions mail t)
          ((partial apply-network-structure-changes model->sim) net-msgs t)
          (assoc :int-mail {})
          (assoc :net-msgs [])
          (assoc :tl t))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim]
    (or (pq/peek-key queue) infinity)))

(declare network-simulator)

(defn default-model->sim
  [model]
  (cond
    (atomic-model?  model) (atomic-simulator  model)
    (network-model? model) (network-simulator model)
    :else                  (throw (ex-info "Unknown model type." {}))))

(defn network-simulator
  "model - A network model.

  Options:

  model->sim - A function that takes a model and returns a new simulator for
  that model. The default pairs atomic models with atomic-simulator and network
  models with network-simulator."
  [model & {:keys [model->sim]
            :or   {model->sim default-model->sim}}]
  (map->NetworkSimulator {:model      model
                          :queue      (pq/priority-queue)
                          :int-mail   {}
                          :net-msgs   []
                          :model->sim model->sim}))
