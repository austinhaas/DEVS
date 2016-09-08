(ns pettomato.devs.network-simulator
  "Muzy, Alexander, and James J. Nutaro. \"Algorithms for efficient
   implementations of the DEVS & DSDEVS abstract simulators.\"
   1st Open International Conference on Modeling & Simulation (OICMS). 2005.
   http://www.i3s.unice.fr/~muzy/Publications/oicms_revised_Nov_21_2005.pdf"
  (:require
   [clojure.set :refer [difference]]
   [pettomato.devs.Simulator :refer [Simulator]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.util :refer [group]]
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
#_
(defn- find-receivers
  "Returns a sequence of [k msg]."
  [A parent src msg]
  ;; Parent is important when an message arrives at a network
  ;; boundary, because we need to know if we are going up out of the
  ;; current network or down into a new network.
  (loop [[s* r*] [[[parent src msg]] []]]
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
        ;; networks forwarding messages.
        (recur (reduce (fn [[s* r*] [p d msg]]
                         (let [m (:model (A d))]
                           (cond
                             (atomic? m)  [s* (conj r* [d msg])]
                             (= d ())     [s* (conj r* [d msg])]
                             (= d p)      [(conj s* [(:parent (A d)) d msg]) r*]
                             (network? m) [(conj s* [d (cons :N (rest d)) msg]) r*]
                             :else        (assert false (str "No receivers for msg: " msg)))))
                       [s*' r*]
                       temp-r*)))
      r*)))
#_
(defn- find-receivers
  "Returns a sequence of [k msg]."
  [A parent src msg]
  ;; Parent is important when an message arrives at a network
  ;; boundary, because we need to know if we are going up out of the
  ;; current network or down into a new network.
  (loop [[s* r*] [[[parent src msg]] []]]
    (if (seq s*)
      (let [[[p s [port val]] & s*'] s*
            k           (if (= p s) :N (first s))
            exec-name   (exec-name (get-in A [p :model]))
            exec-model  (get A (cons exec-name p))]
        (recur (reduce (fn [[s* r*] [d port' t]]
                         (let [d'   (if (= d :N) p (cons d p))
                               val' (t val)
                               msg  [port' val']
                               m    (:model (get A d'))]
                           (cond
                             (atomic? m)  [s* (conj r* [d' msg])]
                             (= d' ())    [s* (conj r* [d' msg])]
                             (= d' p)     [(conj s* [(:parent (get A d')) d' msg]) r*]
                             (network? m) [(conj s* [d' (cons :N (rest d')) msg]) r*]
                             :else        (assert false (str "No receivers for msg: " msg)))))
                       [s*' r*]
                       (get-connections (:state exec-model) k port))))
      r*)))

(defn- find-receivers
  "Returns a sequence of [k port]."
  [pkg parent src port]
  ;; Parent is important when an message arrives at a network
  ;; boundary, because we need to know if we are going up out of the
  ;; current network or down into a new network.
  (let [{:keys [G S]} pkg]
    (loop [[s* r*] [[[parent src port]] []]]
      (if (seq s*)
        (let [[[p s port] & s*'] s*
              k           (if (= p s) :N (first s))
              exec-name   (exec-name (get-in G [p :model]))
              exec-state  (get-in S [(cons exec-name p) :state])
              connections (get-connections exec-state k port)]
          (recur (reduce (fn [[s* r*] [d port' t]]
                           (let [d'   (if (= d :N) p (cons d p))
                                 ;;val' (t val)
                                 m    (get-in G [d' :model])]
                             (cond
                               (atomic? m)  [s* (conj r* [d' port'])]
                               (= d' ())    [s* (conj r* [d' port'])]
                               (= d' p)     [(conj s* [(get-in G [d' :parent]) d' port']) r*]
                               (network? m) [(conj s* [d' (cons :N (rest d')) port']) r*]
                               :else        (assert false (str "No receivers for port: " port')))))
                         [s*' r*]
                         connections)))
        r*))))

(defn- checked-time-advance [m s]
  (let [sigma ((time-advance-fn m) s)]
    (assert (>= sigma 0))
    sigma))

(defn- add-model [pkg path model t]
  (reduce (fn [pkg [p m]]
            (if (network? m)
              (update pkg :G assoc p {:parent (rest p)
                                      :model  m})
              (let [s  (initial-state m)
                    tn (+ t (checked-time-advance m s))]
                (-> pkg
                    (update :G assoc p {:parent (rest p)
                                        :model  m})
                    (update :S assoc p {:state  s
                                        :tl     t
                                        :tn     tn})
                    (update :Q pq/insert tn p)))))
          pkg
          (flatten-model path model)))

(defn- add-model* [pkg p-m* t]
  (reduce (fn [pkg [p m]] (add-model pkg p m t)) pkg p-m*))

(defn- rem-model [pkg path model]
  (reduce (fn [pkg [p m]]
            (if (network? m)
              (update pkg :G dissoc p)
              (-> pkg
                  (update :G dissoc p)
                  (update :S dissoc p)
                  (update :Q pq/delete (get-in pkg [:S p :tn]) p))))
          pkg
          (flatten-model path model)))

(defn- rem-model* [pkg p-m*]
  (reduce (fn [pkg [p m]] (rem-model pkg p m)) pkg p-m*))

(defn- compute [pkg k]
  (let [f (get-in pkg [:G k :model :output-fn])
        s (get-in pkg [:S k :state])]
    (f s)))

(defn- update-sim [d model t msg*]
  (let [{:keys [state tl tn]} d
        state' (if (= t tn)
                 (if (seq msg*)
                   (let [e (- t tl)]
                     ((con-update-fn model) state e msg*))
                   ((int-update-fn model) state))
                 (let [e (- t tl)]
                   ((ext-update-fn model) state e msg*)))]
    (assoc d
           :state state'
           :tl    t
           :tn    (+ t (checked-time-advance model state')))))

(defn- update-sim* [pkg k* k->msg* t]
  (let [{:keys [G S Q]} pkg
        S' (reduce (fn [S k] (update S k update-sim (get-in G [k :model]) t (k->msg* k))) S k*)
        Q' (reduce (fn [Q k] (pq/modify Q (:tn (S k)) k (:tn (S' k)))) Q k*)]
    (assoc pkg :S S' :Q Q')))
#_
(defrecord NetworkSimulator [model state tl tn]
  Simulator
  (init       [this t]
    (let [[G S Q] (add-model [{} (pq/priority-queue)] () model t)]
      (NetworkSimulator. model [G S Q] t (or (pq/peek-key Q) infinity))))
  (int-update [this t]
    (assert (= t tn) (str "(= " t " " tn ")"))
    (let [[G S Q]      state
          imminent   (pq/peek Q)
          input      (for [k  imminent
                           msg (compute (A k))
                           [k' msg'] (find-receivers A (:parent (A k)) k msg)]
                       [k' msg'])
          k->msg*    (group first second [] input)
          k->msg*'   (dissoc k->msg* ())
          receivers  (keys k->msg*')
          {re true
           ra false} (group-by (comp executive? :model A) (into imminent receivers))
          [A' Q']    (-> [G S Q]
                         (update-sim* ra k->msg*' t)
                         (update-sim* re k->msg*' t))
          out        (k->msg* ())
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
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [[G S Q]     state
          input     (for [msg x, [k' msg'] (find-receivers A () () msg)]
                      [k' msg'])
          k->msg*   (group first second [] input)
          receivers (keys k->msg*)
          [A' Q']   (update-sim* [G S Q] receivers k->msg* t)]
      (NetworkSimulator. model [A' Q'] t (or (pq/peek-key Q') infinity))))
  (con-update [this x t]
    (let [[G S Q]      state
          imminent   (if (= t (pq/peek-key Q))
                       (pq/peek Q)
                       [])
          input1     (for [k  imminent
                           msg (compute (A k))
                           [k' msg'] (find-receivers A (:parent (A k)) k msg)]
                       [k' msg'])
          input2     (for [msg x, [k' msg'] (find-receivers A () () msg)]
                       [k' msg'])
          input      (concat input1 input2)
          k->msg*    (group first second [] input)
          k->msg*'   (dissoc k->msg* ())
          receivers  (keys k->msg*')
          {re true
           ra false} (group-by (comp executive? :model A) (into imminent receivers))
          [A' Q']    (-> [G S Q]
                         (update-sim* ra k->msg*' t)
                         (update-sim* re k->msg*' t))
          out        (k->msg* ())
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
  (tl         [this] tl)
  (tn         [this] tn))

(defrecord NetworkSimulator [pkg model tl tn]
  Simulator
  (init       [this t]
    (let [pkg'  {:G {}
                 :S {}
                 :Q (pq/priority-queue)}
          pkg'' (add-model pkg' () model t)]
      (NetworkSimulator. pkg'' model t (or (pq/peek-key (:Q pkg'')) infinity))))
  (int-update [this t]
    (assert (= t tn) (str "(= " t " " tn ")"))
    (let [{:keys [G S Q]} pkg
          imminent   (pq/peek Q)
          input      (for [k  imminent
                           [port val] (compute pkg k)
                           [k' port'] (find-receivers pkg (:parent (G k)) k port)]
                       [k' [port' val]])
          k->msg*    (group first second [] input)
          k->msg*'   (dissoc k->msg* ())
          receivers  (keys k->msg*')
          {re true
           ra false} (group-by (comp executive? :model G) (into imminent receivers))
          pkg'       (-> pkg
                         (update-sim* ra k->msg*' t)
                         (update-sim* re k->msg*' t))
          out        (k->msg* ())
          ;; Update network structures.
          D          (set (for [k re, [k' m] (get-components (:state ((:S pkg)  k)))] [(cons k' (rest k)) m]))
          D'         (set (for [k re, [k' m] (get-components (:state ((:S pkg') k)))] [(cons k' (rest k)) m]))
          D-add      (difference D' D )
          D-rem      (difference D  D')
          pkg''      (-> pkg'
                         (rem-model* D-rem)
                         (add-model* D-add t))]
      [(NetworkSimulator. pkg'' model t (or (pq/peek-key (:Q pkg'')) infinity))
       out]))
  (ext-update [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [{:keys [G S Q]} pkg
          input     (for [[port val] x
                          [k' port'] (find-receivers pkg () () port)]
                      [k' [port' val]])
          k->msg*   (group first second [] input)
          receivers (keys k->msg*)
          pkg'      (update-sim* pkg receivers k->msg* t)]
      (NetworkSimulator. pkg' model t (or (pq/peek-key (:Q pkg')) infinity))))
  (con-update [this x t]
    (let [{:keys [G S Q]} pkg
          imminent   (if (= t (pq/peek-key Q))
                       (pq/peek Q)
                       [])
          input1     (for [k  imminent
                           [port val] (compute pkg k)
                           [k' port'] (find-receivers pkg (:parent (G k)) k port)]
                       [k' [port' val]])
          input2     (for [[port val] x
                           [k' port'] (find-receivers pkg () () port)]
                       [k' [port' val]])
          input      (concat input1 input2)
          k->msg*    (group first second [] input)
          k->msg*'   (dissoc k->msg* ())
          receivers  (keys k->msg*')
          {re true
           ra false} (group-by (comp executive? :model G) (into imminent receivers))
          pkg'       (-> pkg
                         (update-sim* ra k->msg*' t)
                         (update-sim* re k->msg*' t))
          out        (k->msg* ())
          ;; Update network structures.
          D          (set (for [k re, [k' m] (get-components (:state ((:S pkg)  k)))] [(cons k' (rest k)) m]))
          D'         (set (for [k re, [k' m] (get-components (:state ((:S pkg') k)))] [(cons k' (rest k)) m]))
          D-add      (difference D' D )
          D-rem      (difference D  D')
          pkg''      (-> pkg'
                         (rem-model* D-rem)
                         (add-model* D-add t))]
      [(NetworkSimulator. pkg'' model t (or (pq/peek-key (:Q pkg'')) infinity))
       out]))
  (tl         [this] tl)
  (tn         [this] tn))

(defn network-simulator [model]
  (assert (network? model))
  (NetworkSimulator. nil model nil nil))
