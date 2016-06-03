(ns des.network-simulator
  "Muzy, Alexander, and James J. Nutaro. \"Algorithms for efficient
   implementations of the DEVS & DSDEVS abstract simulators.\"
   1st Open International Conference on Modeling & Simulation (OICMS). 2005.
   http://www.i3s.unice.fr/~muzy/Publications/oicms_revised_Nov_21_2005.pdf"
  (:require
   [des.Simulator :refer [Simulator]]
   [des.priority-queue :as pq]
   [pt-lib.number :refer [infinity]]
   [pt-lib.coll :refer [group dissoc-in]]
   [des.atomic-model :refer [atomic?]]
   [des.network-model :refer [network?]]))

(defn- flatten-model
  "Returns a sequence of [path model], where path is a seq of keys
  from the item to the root."
  [root model]
  (loop [open [[root model]]
         acc  (transient [])]
    (if (seq open)
      (let [[[p m] & open'] open]
        (if (network? m)
          (recur (concat open' (for [[k' m'] (:components m)] [(cons k' p) m']))
                 (conj! acc [p m]))
          (recur open' (conj! acc [p m]))))
      (persistent! acc))))

(defn- init-atomic-model [p m t]
  {:path   p
   :parent (rest p)
   :model  m
   :state  (:initial-state m)
   :tl     t
   :tn     (+ t ((:time-advance-fn m) (:initial-state m)))})

(defn- init-network-model [p m]
  {:path   p
   :parent (rest p)
   :model  m})

(defn- compute [d] ((:output-fn (:model d)) (:state d)))

(defn- find-receivers
  "Returns a sequence of [k ev]."
  [A context src ev]
  ;; Context is important when an event arrives at a network boundary,
  ;; because we need to know if we are going up out of the current
  ;; network or down into a new network.
  (loop [[s* r*] [[[context src ev]] []]]
    (if (seq s*)
      (let [[[p s [port val]] & s*'] s*
            ;; Find initial set of receivers.
            k       (if (= p s)
                      :N
                      (first s))
            temp-r* (for [[d port'] (get-in A [p :model :connections k port])]
                      (if (= d :N)
                        [p p [port' val]]
                        [p (cons d p) [port' val]]))]

        ;; We probably don't want this; we may want some models the
        ;; emit messages even if they're aren't any receivers.
        (assert (seq temp-r*) (format "Couldn't find receivers for k: %s port: %s in %s " k port (get-in A [p :model :connections])))

        ;; Sort receivers into actual receivers and new senders for
        ;; networks forwarding events.
        (recur (reduce (fn [[s* r*] [p d ev]]
                         (let [m (:model (A d))]
                           (cond
                             (atomic? m)     [s* (conj r* [d ev])]
                             (= d p)         (let [p' (:parent (A d))
                                                   [port val] ev]
                                               (if (or (nil? p') (empty? p')
                                                       (= port :internal))
                                                 [s* (conj r* [d ev])]
                                                 [(conj s* [p' d ev]) r*]))
                             (or (nil? p)
                                 (empty? p)) [s* (conj r* [d ev])]
                             (network? m)    [(conj s* [d (cons :N (rest d)) ev]) r*]
                             :else           (assert false (A d)))))
                       [s*' r*]
                       temp-r*)))
      r*)))

(defn- add-model [[A Q] path model t]
  (reduce (fn [[A Q] [p m]]
            (if (atomic? m)
              (let [s (init-atomic-model p m t)]
                [(assoc A p s) (pq/add Q (:tn s) p)])
              [(assoc A p (init-network-model p m)) Q]))
          [A Q]
          (flatten-model path model)))

(defn- rem-model [[A Q] path model]
  (reduce (fn [[A Q] [p m]]
            (if (atomic? m)
              [(dissoc A p) (pq/rem Q (:tn (A p)) p)]
              [(dissoc A p) Q]))
          [A Q]
          (flatten-model path model)))

(defn- update-sim [d t ev*]
  (let [{:keys [state model tl tn]} d
        state' (if (= t tn)
                 (if (seq ev*)
                   ((:con-update-fn model) state ev*)
                   ((:int-update-fn model) state))
                 (let [e (- t tl)]
                   ((:ext-update-fn model) state e ev*)))]
    (assoc d
           :state state'
           :tl    t
           :tn    (+ t ((:time-advance-fn model) state')))))

(defn- update-sim* [[A Q] k* k->ev* t]
  (let [A' (reduce (fn [A k] (update A k update-sim t (k->ev* k))) A k*)
        Q' (reduce (fn [Q k] (pq/update Q (:tn (A k)) k (:tn (A' k)))) Q k*)]
   [A' Q']))

(defn- update-network [[A Q] k ev* t]
  (reduce (fn [[A Q] [port val]]
            (case (first val)
              :add-model      (let [[_ k' m] val
                                    A' (assoc-in A [k :model :components k'] m)]
                                (add-model [A' Q] (cons k' k) m t))
              :rem-model      (let [[_ k' m] val
                                    A' (dissoc-in A [k :model :components k'])]
                                (rem-model [A' Q] (cons k' k) m))
              :add-connection (let [[_ sk sp dk dp] val
                                    A' (assoc-in A [k :model :connections sk sp dk] dp)]
                                [A' Q])
              :rem-connection (let [[_ sk sp dk dp] val
                                    A' (dissoc-in A [k :model :connections sk sp dk])]
                                [A' Q])))
          [A Q]
          ev*))

(defn- update-network* [[A Q] k* k->ev* t]
  (reduce (fn [[A Q] k] (update-network [A Q] k (k->ev* k) t)) [A Q] k*))

(defrecord NetworkSimulator [model state tl tn]
  Simulator
  (init       [this t]
    (let [[A Q] (add-model [{} (pq/priority-queue)] (list :root) model t)]
      (NetworkSimulator. model [A Q] t (or (pq/peek-key Q) infinity))))
  (int-update [this t]
    (assert (= t tn) (format "(= %s %s)" t tn))
    (let [[A Q]       state
          imminent    (pq/peek Q)
          output      (for [k  imminent
                            ev (compute (A k))]
                        [k ev])
          input       (for [[k  ev ] output
                            [k' ev'] (find-receivers A (:parent (A k)) k ev)]
                        [k' ev'])
          k->ev*      (group first second [] input)
          receivers   (keys k->ev*)
          {rn true
           ra false}  (group-by (comp network? :model A) receivers)
          {in  true
           out false} (group-by #(= :internal (first %)) (k->ev* (list :root)))
          k->ev*      (assoc k->ev* (list :root) in)
          rn          (if (seq in)
                        rn
                        (remove #(= % (list :root)) rn))

          [A' Q']     (-> [A (pq/pop Q)]
                          (update-sim* (into imminent ra) k->ev* t)
                          (update-network* rn k->ev* t))]
      [(NetworkSimulator. model [A' Q'] t (or (pq/peek-key Q') infinity))
       out]))
  (ext-update [this x t]
    (assert (<= tl t tn) (format "(<= %s %s %s)" tl t tn))
    (let [[A Q]     state
          input     (for [ev x, [k' ev'] (find-receivers A (list :root) (list :root) ev)]
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
