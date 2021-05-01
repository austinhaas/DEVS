(ns pettomato.devs.simulators.rt-network-simulator
  "A real-time network simulator.

  There are two differences between this simulator and the regular network
  simulator:

  1. The time-of-next-event for the component sims is not cached. It is called
  periodically, before advancing the sim clock, in order to give sims an
  opportunity to update their time-of-next-event without having to commit to it
  in advance. This allows a human-in-the-loop, whose future actions cannot be
  anticipated.

  2. All component sims will have their transition function invoked any time
  this network simulator's transition function is invoked, even if they are not
  imminent nor receiving mail. This allows each sim to update to the current sim
  time."
  (:require
   [pettomato.devs.lib.coll :refer [prune]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.mail :refer [merge-mail route-mail sort-mail]]
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.network-model :refer [network-model?]]
   [pettomato.devs.simulator :refer [Simulator initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.rt-atomic-simulator :refer [rt-atomic-simulator]]
   [pettomato.devs.vars :refer [*path*]]))

;; Note that some of these functions are identical to functions in
;; network-simulator, but they depend on distinct implementations of other
;; functions. Only `connect` and `disconnect` could be referenced directly from
;; network-simulator, but that doesn't seem worth the indirection.

(defn apply-transition
  "Invoke a transition for a single simulator.

  network-sim - The network simulator.

  k - The name of the component simulator.

  mail - The local mail (p->vs) for the component simulator.

  t - The current sim-time."
  [network-sim k mail t]
  (let [sim  (get-in network-sim [:k->sim k])
        sim' (binding [*path* (conj *path* k)]
               (transition sim mail t))] ; recursive step
    (-> network-sim
        (assoc-in [:k->sim k] sim'))))

(defn- apply-transitions
  "Invoke a transition across all component simulators.

  network-sim - The network simulator.

  mail - Inbound mail for this simulator (k->p->vs).

  t - The current sim-time."
  [network-sim mail t]
  ;; Note that this could be made to run in parallel.
  (reduce-kv #(apply-transition %1 %2 %3 t)
             network-sim
             mail))

(defn- add-model [find-simulator network-sim k model t]
  (log/tracef "add-model: %s" k)
  (let [simulator (find-simulator k model)
        sim       (simulator model)
        sim       (binding [*path* (conj *path* k)]
                    (initialize sim t))]
    (-> network-sim
        (update :k->sim assoc k sim))))

(defn- rem-model [network-sim k]
  (log/tracef "rem-model: %s" k)
  (let [sim (get-in network-sim [:k->sim k])]
    (-> network-sim
        (update :k->sim dissoc k))))

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

(defn- apply-network-structure-changes [find-simulator network-sim net-msgs t]
  ;; Network structure messages are grouped and processed in a specific order.
  ;; Himmelspach, Jan, and Adelinde M. Uhrmacher. "Processing dynamic PDEVS models."
  ;; The IEEE Computer Society's 12th Annual International Symposium on Modeling, Analysis, and Simulation of Computer and Telecommunications Systems, 2004.
  ;; Section 3.2
  ;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.3385&rep=rep1&type=pdf
  (let [net-msgs   (group-by first net-msgs)
        add-model  (fn [sim [_ k model]] (add-model  find-simulator sim k model t))
        rem-model  (fn [sim [_ k]]       (rem-model  sim k))
        connect    (fn [sim [_ route]]   (connect    sim route))
        disconnect (fn [sim [_ route]]   (disconnect sim route))]
    (as-> network-sim network-sim
      (reduce disconnect network-sim (:disconnect net-msgs))
      (reduce rem-model  network-sim (:rem-model  net-msgs))
      (reduce add-model  network-sim (:add-model  net-msgs))
      (reduce connect    network-sim (:connect    net-msgs)))))

(defn- imminent [k->sim t]
  (for [[k sim] k->sim
        :when (= t (binding [*path* (conj *path* k)]
                     (time-of-next-event sim)))]
    k))

(defrecord RealTimeNetworkSimulator [model k->sim routes int-mail net-msgs find-simulator]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    ;; Assuming initialize will only be called once.
    (as-> sim sim
      ;; Add models.
      (reduce-kv #(add-model find-simulator %1 %2 %3 t) sim (:models model))
      ;; Add routes.
      (reduce connect sim (:routes model))))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (let [tn (time-of-next-event sim)]
      (assert (= t tn)
              (str "synchronization error: (not (= " t " " tn "))"))
      (let [imminent      (imminent k->sim t)
            _             (log/tracef "imminent: %s" (vec imminent)) ;; TODO: Don't convert to vector here; do it in the log fn.
            ;; This could be parallelized.
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
        [sim ext-mail])))
  (transition [sim ext-mail t]
    (log/trace "--- transition ---")
    (let [tl (time-of-last-event sim)
          tn (time-of-next-event sim)]
      (assert (<= tl t tn)
              (str "synchronization error: (not (<= " tl " " t " " tn "))"))
      (let [imminent (imminent k->sim t)
            _        (log/tracef "imminent: %s" (vec imminent)) ;; TODO: Don't convert to vector here; do it in the log fn.
            ;; All component sims are updated, even if they are not imminent.
            all-mail (zipmap (keys k->sim) (repeat {}))
            ext-mail (route-mail routes {:network ext-mail}) ; Assumption: There are no routes from :network to :network.
            _        (log/tracef "ext-mail: %s" ext-mail)
            mail     (merge-mail all-mail int-mail ext-mail)
            _        (log/tracef "mail: %s" mail)]
        (-> sim
            (apply-transitions mail t)
            ((partial apply-network-structure-changes find-simulator) net-msgs t)
            (assoc :int-mail {})
            (assoc :net-msgs [])))))
  (time-of-last-event [sim]
    (reduce max (map (fn [[k sim]] (binding [*path* (conj *path* k)]
                                     (time-of-last-event sim)))
                     k->sim)))
  (time-of-next-event [sim]
    (reduce min (map (fn [[k sim]] (binding [*path* (conj *path* k)]
                                     (time-of-next-event sim)))
                     k->sim))))

(declare rt-network-simulator)

(defn default-find-simulator
  "A function that takes two args: a name and a model, and returns a simulator
  for that model.

  This version maps atomic-models to rt-atomic-simulators and network-models to
  rt-network-simulators."
  [k model]
  (cond
    (atomic-model?  model) rt-atomic-simulator
    (network-model? model) rt-network-simulator
    :else                  (throw (ex-info "Unknown model type." {}))))

(defn rt-network-simulator
  "Wrap a real-time network model in a real-time network simulator.

  Args:

    model - A real-time network model.

  Optional keyword args:

    find-simulator - A function that takes two args: a name and a model, and returns
    a simulator for that model. The default maps atomic-models to rt-atomic-simulators
    and network-models to rt-network-simulators.

  Returns:

    A simulator.

  The network's component models can request network structure changes by
  sending special messages from a :structure port.

  The following messages are supported:

  [:add-model model-name model]

  [:rem-model model-name]

  [:connect route]

  [:disconnect route]"
  [model & {:keys [find-simulator]
            :or   {find-simulator default-find-simulator}}]
  (map->RealTimeNetworkSimulator {:model          model
                                  :int-mail       {}
                                  :net-msgs       []
                                  :find-simulator find-simulator}))
