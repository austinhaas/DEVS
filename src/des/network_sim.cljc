(ns des.network-sim
  "Muzy, Alexander, and James J. Nutaro. \"Algorithms for efficient
   implementations of the DEVS & DSDEVS abstract simulators.\"
   1st Open International Conference on Modeling & Simulation (OICMS). 2005.
   http://www.i3s.unice.fr/~muzy/Publications/oicms_revised_Nov_21_2005.pdf"
  (:require
   [des.priority-queue :as pq]
   [pt-lib.number :refer [infinity]]
   [pt-lib.coll :refer [group dissoc-in]]
   [des.model :refer [atomic? network?]]))

(defn- flatten-model
  "Returns a sequence of [parent k model]."
  [parent k model]
  (loop [open [[parent k model]]
         acc  (transient [])]
    (if (seq open)
      (let [[[p k m] & open'] open]
        (if (network? m)
          (recur (concat open' (for [[k' m'] (:components m)] [k k' m']))
                 (conj! acc [p k m]))
          (recur open' (conj! acc [p k m]))))
      (persistent! acc))))

(defn- init-atomic-model [p m t]
  {:parent p
   :model  m
   :state  (:initial-state m)
   :tl     t
   :tn     (+ t ((:time-advance-fn m) (:initial-state m)))})

(defn- init-network-model [p m]
  (assoc m :parent p))

(defn- compute [d] ((:output-fn (:model d)) (:state d)))

(defn- find-receivers
  "Returns a sequence of [k ev]."
  [A parent src ev]
  ;; Note that parent here refers to the context for the event. This
  ;; is important when an event arrives at a network boundary, because
  ;; we need to know if we are going up out of the current network or
  ;; down into a new network.
  (loop [[s* r*] [[[parent src ev]] []]]
    (if (seq s*)
      (let [[[p s [port val]] & s*'] s*
            ;; Find initial set of receivers.
            temp-r* (for [[d port'] (get-in A [p :connections s port])]
                      (if (= d p)
                        [(:parent (A d)) d [port' val]]
                        [p d [port' val]]))]
        ;; Sort receivers into actual receivers and new senders for
        ;; networks forwarding events.
        (recur (reduce (fn [[s* r*] [p d ev]]
                         (let [x (A d)
                               m (:model x)]
                           (cond
                             (atomic? m)  [s* (conj r* [d ev])]
                             (= d p)      [(conj s* [(:parent (A d)) d ev]) r*]
                             (nil? p)     [s* (conj r* [d ev])]
                             (network? m) [(conj s* [d d ev]) r*])))
                       [s*' r*]
                       temp-r*)))
      r*)))

(defn- add-model [[A Q] parent k model t]
  (reduce (fn [[A Q] [p k m]]
            (if (atomic? m)
              (let [s (init-atomic-model p m t)]
                [(assoc A k s) (pq/add Q (:tn s) k)])
              [(assoc A k (init-network-model p m)) Q]))
          [A Q]
          (flatten-model parent k model)))

(defn- rem-model [[A Q] parent k model]
  (reduce (fn [[A Q] [p k m]]
            (if (atomic? m)
              [(dissoc A k) (pq/rem Q (:tn (A k)) k)]
              [(dissoc A k) Q]))
          [A Q]
          (flatten-model parent k model)))

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
  (reduce (fn [A [port val]]
            (case (first val)
              :add-model      (let [[_ k' m] val
                                    A'     (assoc-in A [k :components k'] m)]
                                (add-model [A' Q] k k' m t))
              :rem-model      (let [[_ k' m] val
                                    A'     (dissoc-in A [k :components k'])]
                                (rem-model [A' Q] k k' m))
              :add-connection (let [[_ sk sp dk dp] val]
                                [(assoc-in A [k :connections sk sp dk] dp) Q])
              :rem-connection (let [[_ sk sp dk dp] val]
                                [(dissoc-in A [k :connections sk sp dk] dp) Q])))
          [A Q]
          ev*))

(defn- update-network* [[A Q] k* k->ev* t]
  (reduce (fn [[A Q] k] (update-network [A Q] k (k->ev* k) t)) [A Q] k*))

(def op-syms #{:add-model :rem-model :add-connection :rem-connection})

(defn network-sim [model]
  {:model model
   :state nil
   :tl    nil
   :tn    nil})

(defn init [sim t]
  (let [[A Q] (add-model [{} (pq/priority-queue)] nil :N (:model sim) t)
        tl    t
        tn    (or (pq/peek-key Q) infinity)]
    (assoc sim :tl tl :tn tn :state [A Q])))

(defn int-update [sim t]
  (assert (= t (:tn sim)) (format "(= %s %s)" t (:tn sim)))
  (let [[A Q]       (:state sim)
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
         ra false}  (group-by (comp network? A) receivers)
        output      (k->ev* :N)

        ;; This breaks b/c non-network change messages may not be vectors.
        ;; {ops true
        ;;  out false} (group-by (comp boolean op-syms first second) (k->ev* :N))
        ops []
        out (k->ev* :N)
        [A' Q']     (-> [A (pq/pop Q)]
                        (update-sim* (into imminent ra) k->ev* t)
                        (update-network* rn {:N ops} t))] ;; Need to know which network is being updated.
    [(assoc sim :tl t :tn (or (pq/peek-key Q') infinity) :state [A' Q'])
     out]))

(defn ext-update [sim x t]
  (assert (<= (:tl sim) t (:tn sim)) (format "(<= %s %s %s)" (:tl sim) t (:tn sim)))
  (let [[A Q]     (:state sim)
        input     (for [ev x, [k' ev'] (find-receivers A :N :N ev)]
                    [k' ev'])
        k->ev*    (group first second [] input)
        receivers (keys k->ev*)
        [A' Q']   (update-sim* [A Q] receivers k->ev* t)]
    (assoc sim :tl t :tn (or (pq/peek-key Q') infinity) :state [A' Q'])))

(defn tl [sim] (:tl sim))
(defn tn [sim] (:tn sim))
