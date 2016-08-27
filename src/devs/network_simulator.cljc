(ns pettomato.devs.network-simulator
  "Muzy, Alexander, and James J. Nutaro. \"Algorithms for efficient
   implementations of the DEVS & DSDEVS abstract simulators.\"
   1st Open International Conference on Modeling & Simulation (OICMS). 2005.
   http://www.i3s.unice.fr/~muzy/Publications/oicms_revised_Nov_21_2005.pdf"
  (:require
   [clojure.set :refer [difference]]
   [pettomato.devs.Simulator :refer [Simulator]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.lib.number :refer [infinity]]
   [pettomato.lib.coll :refer [group]]
   [pettomato.devs.models :refer [atomic? executive? network?
                        initial-state int-update-fn ext-update-fn con-update-fn output-fn time-advance-fn
                        get-components get-connections
                        exec-name exec-model]]))

(defn- flatten-model
  "Returns a sequence of [path model], where path is a seq of keys
  from the item to the root."
  [root model]
  (loop [open [[root model]]
         acc  (transient [])]
    (if (seq open)
      (let [[[p m] & open'] open]
        (if (network? m)
          (recur (concat open' (for [[k' m'] (get-components (initial-state (exec-model m)))]
                                 [(cons k' p) m']))
                 (-> acc (conj! [p m]) (conj! [(cons (exec-name m) p) (exec-model m)])))
          (recur open' (conj! acc [p m]))))
      (persistent! acc))))

(defn- find-receivers
  "Returns a sequence of [k ev]."
  [A parent src ev]
  ;; Parent is important when an event arrives at a network boundary,
  ;; because we need to know if we are going up out of the current
  ;; network or down into a new network.
  (loop [[s* r*] [[[parent src ev]] []]]
    (if (seq s*)
      (let [[[p s [port val]] & s*'] s*
            ;; Find initial set of receivers.
            k           (if (= p s) :N (first s))
            exec-name   (exec-name (get-in A [p :model]))
            exec-model  (A (cons exec-name p))
            temp-r*     (for [[d port' t] (get-connections (:state exec-model) k port)]
                          (let [d'   (if (= d :N) p (cons d p))
                                val' (t val)]
                            [p d' [port' val']]))]
        ;; Sort receivers into actual receivers and new senders for
        ;; networks forwarding events.
        (recur (reduce (fn [[s* r*] [p d ev]]
                         (let [m (:model (A d))]
                           (cond
                             (atomic? m)  [s* (conj r* [d ev])]
                             (= d ())     [s* (conj r* [d ev])]
                             (= d p)      [(conj s* [(:parent (A d)) d ev]) r*]
                             (network? m) [(conj s* [d (cons :N (rest d)) ev]) r*]
                             :else        (assert false (format "No receivers for ev: %s" ev)))))
                       [s*' r*]
                       temp-r*)))
      r*)))

(defn- checked-time-advance [m s]
  (let [sigma ((time-advance-fn m) s)]
    (assert (>= sigma 0))
    sigma))

(defn- init-atomic-model [path m t]
  {:path   path
   :parent (rest path)
   :model  m
   :state  (initial-state m)
   :tl     t
   :tn     (+ t (checked-time-advance m (initial-state m)))})

(defn- init-network-model [p m]
  {:path   p
   :parent (rest p)
   :model  m})

(defn- add-model [[A Q] path model t]
  (reduce (fn [[A Q] [p m]]
            (if (network? m)
              [(assoc A p (init-network-model p m)) Q]
              (let [s (init-atomic-model p m t)]
                [(assoc A p s) (pq/add Q (:tn s) p)])))
          [A Q]
          (flatten-model path model)))

(defn- add-model* [[A Q] p-m* t]
  (reduce (fn [[A Q] [p m]] (add-model [A Q] p m t)) [A Q] p-m*))

(defn- rem-model [[A Q] path model]
  (reduce (fn [[A Q] [p m]]
            (if (network? m)
              [(dissoc A p) Q]
              [(dissoc A p) (pq/rem Q (:tn (A p)) p)]))
          [A Q]
          (flatten-model path model)))

(defn- rem-model* [[A Q] p-m*]
  (reduce (fn [[A Q] [p m]] (rem-model [A Q] p m)) [A Q] p-m*))

(defn- compute [d] ((output-fn (:model d)) (:state d)))

(defn- update-sim [d t ev*]
  (let [{:keys [state model tl tn]} d
        state' (if (= t tn)
                 (if (seq ev*)
                   (let [e (- t tl)]
                     ((con-update-fn model) state e ev*))
                   ((int-update-fn model) state))
                 (let [e (- t tl)]
                   ((ext-update-fn model) state e ev*)))]
    (assoc d
           :state state'
           :tl    t
           :tn    (+ t (checked-time-advance model state')))))

(defn- update-sim* [[A Q] k* k->ev* t]
  (let [A' (reduce (fn [A k] (update A k update-sim t (k->ev* k))) A k*)
        Q' (reduce (fn [Q k] (pq/update Q (:tn (A k)) k (:tn (A' k)))) Q k*)]
   [A' Q']))

(defrecord NetworkSimulator [model state tl tn]
  Simulator
  (init       [this t]
    (let [[A Q] (add-model [{} (pq/priority-queue)] () model t)]
      (NetworkSimulator. model [A Q] t (or (pq/peek-key Q) infinity))))
  (int-update [this t]
    (assert (= t tn) (format "(= %s %s)" t tn))
    (let [[A Q]      state
          imminent   (pq/peek Q)
          output     (for [k  imminent
                           ev (compute (A k))]
                       [k ev])
          input      (for [[k  ev ] output
                           [k' ev'] (find-receivers A (:parent (A k)) k ev)]
                       [k' ev'])
          k->ev*     (group first second [] input)
          k->ev*'    (dissoc k->ev* ())
          receivers  (keys k->ev*')
          {re true
           ra false} (group-by (comp executive? :model A) receivers)
          [A' Q']    (-> [A Q]
                         (update-sim* (into imminent ra) k->ev*' t)
                         (update-sim* re k->ev*' t))
          out        (k->ev* ())
          ;; Update network structures.
          D          (set (for [k re, [k' m] (get-components (:state (A  k)))] [(cons k' (rest k)) m]))
          D'         (set (for [k re, [k' m] (get-components (:state (A' k)))] [(cons k' (rest k)) m]))
          D-add      (difference D' D )
          D-rem      (difference D  D')
          [A' Q']    (-> [A' Q']
                         (rem-model* D-rem)
                         (add-model* D-add t))]
      [(NetworkSimulator. model [A' Q'] t (or (pq/peek-key Q') infinity))
       out]))
  (ext-update [this x t]
    (assert (<= tl t tn) (format "(<= %s %s %s)" tl t tn))
    (let [[A Q]     state
          input     (for [ev x, [k' ev'] (find-receivers A () () ev)]
                      [k' ev'])
          k->ev*    (group first second [] input)
          receivers (keys k->ev*)
          [A' Q']   (update-sim* [A Q] receivers k->ev* t)]
      (NetworkSimulator. model [A' Q'] t (or (pq/peek-key Q') infinity))))
  (tl         [this] tl)
  (tn         [this] tn))

(defn network-simulator [model]
  (assert (network? model))
  (NetworkSimulator. model nil nil nil))
