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
   [pettomato.devs.util :refer [group-cons]]
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
  "Returns a sequence of [k port]."
  [P M C parent src port]
  ;; Parent is important when an message arrives at a network
  ;; boundary, because we need to know if we are going up out of the
  ;; current network or down into a new network.
  (loop [[s* r*] [[[parent src port []]] []]]
    (if (seq s*)
      (let [[[p s port t*] & s*'] s*
            k           (if (= p s) :N (first s))
            exec-name   (exec-name (get M p))
            connections (for [[k m] (get-in C [(cons exec-name p) k port])
                              [p t] m]
                          [k p t])]
        (recur (reduce (fn [[s* r*] [d port' t]]
                         (let [d'   (if (= d :N) p (cons d p))
                               ;;val' (t val)
                               m    (get M d')]
                           (cond
                             (atomic? m)  [s* (conj r* [d' port' (conj t* t)])]
                             (= d' ())    [s* (conj r* [d' port' (conj t* t)])]
                             (= d' p)     [(conj s* [(get P d') d' port' (conj t* t)]) r*]
                             (network? m) [(conj s* [d' (cons :N (rest d')) port' (conj t* t)]) r*]
                             :else        (assert false (str "No receivers for port: " port')))))
                       [s*' r*]
                       connections)))
      r*)))

(defn- checked-time-advance [m s]
  (let [sigma ((time-advance-fn m) s)]
    (assert (>= sigma 0))
    sigma))

(defn- add-model [pkg path model t]
  (reduce (fn [pkg [p m]]
            (cond
              (network? m)   (-> pkg
                                 (update :P assoc p (rest p))
                                 (update :M assoc p m))
              (executive? m) (let [s  (initial-state m)
                                   tn (+ t (checked-time-advance m s))]
                               (-> pkg
                                   (update :P assoc p (rest p))
                                   (update :M assoc p m)
                                   (update :S assoc p {:state  s
                                                       :tl     t
                                                       :tn     tn})
                                   (update :C assoc p (get s :connections))
                                   (update :Q pq/insert tn p)
                                   (assoc :find-receivers-m (memoize find-receivers))))
              :else          (let [s  (initial-state m)
                                   tn (+ t (checked-time-advance m s))]
                               (-> pkg
                                   (update :P assoc p (rest p))
                                   (update :M assoc p m)
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
            (cond
              (network? m)   (-> pkg
                                 (update :P dissoc p)
                                 (update :M dissoc p))
              (executive? m) (-> pkg
                                 (update :P dissoc p)
                                 (update :M dissoc p)
                                 (update :S dissoc p)
                                 (update :C dissoc p)
                                 (update :Q pq/delete (get-in pkg [:S p :tn]) p)
                                 (assoc :find-receivers-m (memoize find-receivers)))
              :else          (-> pkg
                                 (update :P dissoc p)
                                 (update :M dissoc p)
                                 (update :S dissoc p)
                                 (update :Q pq/delete (get-in pkg [:S p :tn]) p))))
          pkg
          (flatten-model path model)))

(defn- rem-model* [pkg p-m*]
  (reduce (fn [pkg [p m]] (rem-model pkg p m)) pkg p-m*))

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
  (let [{:keys [M S Q]} pkg
        S' (reduce (fn [S k] (update S k update-sim (get M k) t (k->msg* k))) S k*)
        Q' (pq/modify* Q (for [k k*] [(:tn (S k)) k (:tn (S' k))]))]
    (assoc pkg :S S' :Q Q')))

(defn- rcall [x y] (y x))

(defrecord NetworkSimulator [pkg model tl tn]
  Simulator
  (init       [this t]
    (let [pkg'  {:P {}
                 :M {}
                 :S {}
                 :C {}
                 :Q (pq/init)
                 :find-receivers-m nil}
          pkg'' (add-model pkg' () model t)]
      (NetworkSimulator. pkg'' model t (or (pq/peek-key (:Q pkg'')) infinity))))
  (int-update [this t]
    (assert (= t tn) (str "(= " t " " tn ")"))
    (let [{:keys [P M C S Q find-receivers-m]} pkg
          imminent   (pq/peek Q)
          input      (for [k             imminent
                           :let [k-parent (P k)]
                           [port val]    ((get-in M [k :output-fn]) (get-in S [k :state]))
                           [k' port' t*] (find-receivers-m P M C k-parent k port)]
                       (let [val' (reduce rcall val t*)]
                         [k' [port' val']]))
          k->msg*    (group-cons first second input)
          k->msg*'   (dissoc k->msg* ())
          receivers  (keys k->msg*')
          {re true
           ra false} (group-by (comp executive? M) (into imminent receivers))
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
                         (add-model* D-add t))
          C'         (reduce (fn [C k]
                               (let [connections (get-in pkg'' [:S k :state :connections])]
                                 (assoc C k connections)))
                             (:C pkg'')
                             re)
          pkg''      (if (not= C' (:C pkg''))
                       (assoc pkg''
                              :C C'
                              :find-receivers-m (memoize find-receivers))
                       pkg'')]
      [(NetworkSimulator. pkg'' model t (or (pq/peek-key (:Q pkg'')) infinity))
       out]))
  (ext-update [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [{:keys [P M C find-receivers-m]} pkg
          input     (for [[port val] x
                          [k' port' t*] (find-receivers-m P M C () () port)]
                      (let [val' (reduce rcall val t*)]
                        [k' [port' val']]))
          k->msg*   (group-cons first second input)
          receivers (keys k->msg*)
          pkg'      (update-sim* pkg receivers k->msg* t)]
      (NetworkSimulator. pkg' model t (or (pq/peek-key (:Q pkg')) infinity))))
  (con-update [this x t]
    (let [{:keys [P M C S Q find-receivers-m]} pkg
          imminent   (if (= t (pq/peek-key Q))
                       (pq/peek Q)
                       [])
          input1     (for [k             imminent
                           :let [k-parent (P k)]
                           [port val]    ((get-in M [k :output-fn]) (get-in S [k :state]))
                           [k' port' t*] (find-receivers-m P M C k-parent k port)]
                       (let [val' (reduce rcall val t*)]
                         [k' [port' val']]))
          input2     (for [[port val] x
                           [k' port' t*] (find-receivers-m P M C () () port)]
                       (let [val' (reduce rcall val t*)]
                         [k' [port' val']]))
          input      (concat input1 input2)
          k->msg*    (group-cons first second input)
          k->msg*'   (dissoc k->msg* ())
          receivers  (keys k->msg*')
          {re true
           ra false} (group-by (comp executive? M) (into imminent receivers))
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
                         (add-model* D-add t))
          C'         (reduce (fn [C k]
                               (let [connections (get-in pkg'' [:S k :state :connections])]
                                 (assoc C k connections)))
                             (:C pkg'')
                             re)
          pkg''      (if (not= C' (:C pkg''))
                       (assoc pkg''
                              :C C'
                              :find-receivers-m (memoize find-receivers))
                       pkg'')]
      [(NetworkSimulator. pkg'' model t (or (pq/peek-key (:Q pkg'')) infinity))
       out]))
  (tl         [this] tl)
  (tn         [this] tn))

(defn network-simulator [model]
  (assert (network? model))
  (NetworkSimulator. nil model nil nil))
