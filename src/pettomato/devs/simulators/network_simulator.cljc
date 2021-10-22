(ns pettomato.devs.simulators.network-simulator
  "A network simulator."
  (:require
   [pettomato.devs.lib.coll :refer [prune]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :refer [merge-mail route-mail sort-mail]]
   [pettomato.devs.lib.priority-queue :as pq]
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.network-model :refer [network-model?]]
   [pettomato.devs.simulator :refer [Simulator initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.vars :refer [*path*]]))

(defn- add-model [find-simulator network-sim k t model]
  (log/tracef "add-model: %s" k)
  (let [simulator (find-simulator k model)
        sim       (simulator model)
        sim       (binding [*path* (conj *path* k)]
                    (initialize sim t))
        tn        (binding [*path* (conj *path* k)]
                    (time-of-next-event sim))]
    (-> network-sim
        (update :k->sim assoc k sim)
        (update :queue pq/insert tn k))))

(defn- rem-model [network-sim k]
  (log/tracef "rem-model: %s" k)
  (let [sim (get-in network-sim [:k->sim k])
        tn  (binding [*path* (conj *path* k)]
              (time-of-next-event sim))]
    (-> network-sim
        (update :k->sim dissoc k)
        (update :queue pq/delete tn k))))

(defn- connect [network-sim [sk sp rk rp f]]
  (log/tracef "connect: %s" [sk sp rk rp f])
  (let [f (or f identity)] ;; f is optional; defaults to identity.
    (-> network-sim
        (update-in [:routes sk sp rk rp] (fnil conj #{}) f))))

(defn- disconnect [network-sim [sk sp rk rp f]]
  (log/tracef "disconnect: %s" [sk sp rk rp f])
  (let [f (or f identity)]  ;; f is optional; defaults to identity.
    (-> network-sim
        (update-in [:routes sk sp rk rp] disj f)
        (update :routes prune [sk sp rk rp]))))

(defn apply-transition
  "Invoke a transition for a single simulator.

  network-sim - The network simulator.

  k - The name of the component simulator.

  t - The current sim-time.

  mail - The local mail (p->vs) for the component simulator."
  [network-sim k t mail]
  (let [sim  (get-in network-sim [:k->sim k])
        sim' (binding [*path* (conj *path* k)]
               (transition sim mail t))  ; recursive step
        tn   (binding [*path* (conj *path* k)]
               (time-of-next-event sim)) ; Previously scheduled time; (<= t tn).
        tn'  (binding [*path* (conj *path* k)]
               (time-of-next-event sim'))]
    (-> network-sim
        (assoc-in [:k->sim k] sim')
        (update :queue pq/change-priority tn k tn'))))

(defn- apply-transitions
  "Invoke a transition across all component simulators.

  network-sim - The network simulator.

  t - The current sim-time.

  mail - Inbound mail for this simulator (k->p->vs)."
  [network-sim t mail]
  ;; This could run in parallel.
  (reduce-kv #(apply-transition %1 %2 t %3)
             network-sim
             mail))

(defn- apply-petitions [find-simulator network-sim t petitions]
  ;; Network structure messages are grouped and processed in a specific
  ;; order. See Himmelspach, Jan, and Adelinde M. Uhrmacher. "Processing dynamic
  ;; PDEVS models." The IEEE Computer Society's 12th Annual International
  ;; Symposium on Modeling, Analysis, and Simulation of Computer and
  ;; Telecommunications Systems, 2004. Section 3.2
  ;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.3385&rep=rep1&type=pdf
  (let [petitions  (group-by first petitions)
        add-model  (fn [sim [_ k model]] (add-model  find-simulator sim k t model))
        rem-model  (fn [sim [_ k]]       (rem-model  sim k))
        connect    (fn [sim [_ route]]   (connect    sim route))
        disconnect (fn [sim [_ route]]   (disconnect sim route))]
    (as-> network-sim network-sim
      (reduce disconnect network-sim (:disconnect petitions))
      (reduce rem-model  network-sim (:rem-model  petitions))
      (reduce add-model  network-sim (:add-model  petitions))
      (reduce connect    network-sim (:connect    petitions)))))

(defrecord NetworkSimulator [model k->sim routes queue int-mail petitions find-simulator tl tn]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    ;; Assuming initialize will only be called once.
    (as-> sim sim
      ;; Add models.
      (reduce-kv #(add-model find-simulator %1 %2 t %3) sim (:models model))
      ;; Add routes.
      (reduce connect sim (:routes model))
      ;; Cache tl.
      (assoc sim :tl (apply h/max (map (fn [[k sim]]
                                         (binding [*path* (conj *path* k)]
                                           (time-of-last-event sim)))
                                       (:k->sim sim))))
      (assoc sim :tn (or (pq/peek-key (:queue sim)) h/infinity))))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (assert (h/= t tn) (str "synchronization error: (not (= " t " " tn "))"))
    (let [imminent      (pq/peek queue)
          _             (log/tracef "imminent: %s" (vec imminent)) ;; TODO: Don't convert to vector here; do it in the log fn.
          ;; This could be parallelized.
          sim-mail      (map (fn [k]
                               (binding [*path* (conj *path* k)]
                                 (collect-mail (k->sim k) t))) ; recursive step
                             imminent)
          k->sim'       (zipmap imminent (map first  sim-mail))
          outbound-mail (zipmap imminent (map second sim-mail))
          _             (log/tracef "outbound-mail: %s" outbound-mail)
          inbound-mail  (route-mail routes outbound-mail)
          _             (log/tracef " inbound-mail: %s" inbound-mail)
          [int-mail
           ext-mail
           petitions]   (sort-mail inbound-mail)
          sim           (-> sim
                            (update :k->sim merge k->sim')
                            (assoc :int-mail  int-mail
                                   :petitions petitions))]
      [sim ext-mail]))
  (transition [sim ext-mail t]
    (log/trace "--- transition ---")
    (assert (h/<= tl t tn) (str "synchronization error: (not (<= " tl " " t " " tn "))"))
    (let [imminent (if (h/= t tn) (pq/peek queue) [])
          _        (log/tracef "imminent: %s" (vec imminent)) ;; TODO: Don't convert to vector here; do it in the log fn.
          ;; The transitions are "mail-driven", so the members of the imminent
          ;; set are primed with an empty bag.
          imm-mail (zipmap imminent (repeat {}))
          ext-mail (route-mail routes {:network ext-mail})    ; Assumption: There are no routes from :network to :network.
          _        (log/tracef "ext-mail: %s" ext-mail)
          mail     (merge-mail imm-mail int-mail ext-mail)
          _        (log/tracef "mail: %s" mail)
          sim      (apply-transitions sim t mail)
          sim      (apply-petitions find-simulator sim (h/+ t h/epsilon) petitions)]
      (assoc sim
             :int-mail  {}
             :petitions []
             :tl        t
             :tn        (or (pq/peek-key (:queue sim))
                            h/infinity))))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(declare network-simulator)

(defn default-find-simulator
  "A function that takes two args: a name and a model, and returns a simulator
  for that model.

  This version maps atomic-models to atomic-simulators and network-models to
  network-simulators."
  [k model]
  (cond
    (atomic-model?  model) atomic-simulator
    (network-model? model) network-simulator
    :else                  (throw (ex-info "Unknown model type." {}))))

(defn network-simulator
  "Wrap a network model in a network simulator.

  Args:

    model - A network model.

  Optional keyword args:

    find-simulator - A function that takes two args: a name and a model, and returns
    a simulator for that model. The default maps atomic-models to atomic-simulators
    and network-models to network-simulators.

  Returns:

    A simulator.

  The network's component models can request network structure changes by
  sending special messages to a :structure port.

  The following messages are supported:

  [:add-model model-name model]

  [:rem-model model-name]

  [:connect route]

  [:disconnect route]"
  [model & {:keys [find-simulator]
            :or   {find-simulator default-find-simulator}}]
  (map->NetworkSimulator {:model          model
                          :queue          (pq/priority-queue h/comparator)
                          :find-simulator find-simulator}))
