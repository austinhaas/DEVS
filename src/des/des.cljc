(ns des.des
  (:require
   [clojure.core.async :as async :refer [timeout close! alts! go >!]]
   [pt-lib.number :refer [infinity]]
   [pt-lib.date :refer [now]]))

(defn model [initial-state int-update-fn ext-update-fn output-fn time-advance-fn]
  {:type            ::atomic
   :initial-state   initial-state
   :int-update-fn   int-update-fn
   :ext-update-fn   ext-update-fn
   :output-fn       output-fn
   :time-advance-fn time-advance-fn})

(defn coupled-model [components ext-input-coupling ext-output-coupling int-coupling select]
  {:type                ::coupled
   :components          components
   :ext-input-coupling  ext-input-coupling
   :ext-output-coupling ext-output-coupling
   :int-coupling        int-coupling
   :select              select})

(defprotocol Simulator
  (init       [this t])
  (int-update [this t])
  (ext-update [this x t])
  (last-time  [this])
  (next-time  [this]))

(defrecord Sim [model last-time next-time state]
  Simulator
  (init [this t]
    (let [state'     (:initial-state model)
          last-time' t
          next-time' (+ t ((:time-advance-fn model) state'))]
     (Sim. model last-time' next-time' state')))
  (int-update [this t]
    (assert (= t next-time) (format "(= %s %next-time)" t next-time))
    (let [y          ((:output-fn model) state)
          state'     ((:int-update-fn model) state)
          last-time' t
          next-time' (+ t ((:time-advance-fn model) state'))]
      [(Sim. model last-time' next-time' state') y]))
  (ext-update [this x t]
    (assert (<= last-time t next-time) (format "(<= %s %s %s)" last-time t next-time))
    (let [e          (- t last-time)
          state'     ((:ext-update-fn model) state e x)
          last-time' t
          next-time' (+ t ((:time-advance-fn model) state'))]
      (Sim. model last-time' next-time' state')))
  (last-time [this] last-time)
  (next-time [this] next-time))

(defprotocol Coordinator
  ;; A model can only add components to its parent coupled model.
  (add-model    [this k m])
  ;; A model can only remove itself and its siblings.
  (rem-model    [this k])
  ;; A model can only add couplings involving itself, its parent, or
  ;; its siblings.
  (add-coupling [this c1 c1-output-port c2 c2-input-port])
  ;; A model can only remove couplings involving itself, its parent,
  ;; and its siblings.
  (rem-coupling [this c1 c1-output-port c2 c2-input-port])
  ;; A model can only add/remove ports of itself and its siblings.
  (add-input-port  [this c p])
  (rem-input-port  [this c p])
  (add-output-port [this c p])
  (rem-output-port [this c p]))

(declare model->simulator)

(defrecord Coord [model last-time next-time simulators event-list]
  Simulator
  (init       [this t]
    (let [simulators' (reduce-kv (fn [m k sim] (assoc m k (init sim t)))
                                 {}
                                 simulators)
          event-list' (into event-list (for [[k sim] simulators'] [(next-time sim) k]))
          last-time'  t
          next-time'  (ffirst event-list')]
      (Coord. model last-time' next-time' simulators' event-list')))
  (int-update [this t]
    (assert (= t next-time) (format "(= %s %s)" t next-time))
    (let [[t k]       (first event-list)
          sim         (simulators k)
          [sim' y]    (int-update sim t)
          simulators' (assoc simulators k sim')
          event-list' (-> event-list
                          (disj [t k])
                          (conj [(next-time sim') k]))
          last-time'  t]
      (if (nil? y)
        (let [next-time' (ffirst event-list')]
          [(Coord. model last-time' next-time' simulators' event-list')
           nil])
        (let [[p v]        y
              receivers    (for [[[k1 p1] [k2 p2]] (:int-coupling model)
                                 :when (= [k1 p1] [k p])]
                             [k2 p2])
              simulators'' (reduce (fn [m [k p]] (update m k ext-update [p v] t))
                                   simulators'
                                   receivers)
              event-list'' (reduce (fn [el k]
                                     (-> el
                                         (disj [(next-time (simulators'  k)) k])
                                         (conj [(next-time (simulators'' k)) k])))
                                   event-list'
                                   (map first receivers))
              ext-out      (for [[[k1 p1] [k2 p2]] (:ext-output-coupling model)
                                 :when (= [k1 p1] [k p])]
                             [p2 v])
              next-time'   (ffirst event-list'')
              ;; Temporary hack.
              _       (assert (= 1 (count ext-out)))
              ext-out (first ext-out)]
          [(Coord. model last-time' next-time' simulators'' event-list'')
           ext-out]))))
  (ext-update [this x t]
    (assert (<= last-time t next-time) (format "(<= %s %s %s)" last-time t next-time))
    (let [[p v]       x
          receivers   (for [[[k1 p1] [k2 p2]] (:ext-input-coupling model)
                            :when (= p1 p)]
                        [k2 p2])
          simulators' (reduce (fn [m [k p]] (update m k ext-update [p v] t))
                              simulators
                              receivers)
          event-list' (reduce (fn [el k]
                                (-> el
                                    (disj [(next-time (simulators  k)) k])
                                    (conj [(next-time (simulators' k)) k])))
                              event-list
                              (map first receivers))
          last-time'  t
          next-time'  (ffirst event-list')]
      (Coord. model last-time' next-time' simulators' event-list')))
  Coordinator
  (add-model    [this k m]
    (assert (not (contains? simulators k)) "Duplicate key.")
    (let [model'      (update model :components assoc k m)
          sim         (-> (model->simulator m)
                          (init last-time))
          simulators' (assoc simulators k sim)
          event-list' (conj event-list [(next-time sim) k])
          next-time'  (ffirst event-list')]
      (Coord. model' last-time next-time' simulators' event-list')))
  (rem-model    [this k]
    (let [model'      (-> model
                          (update :components dissoc k)
                          (update :ext-input-coupling  #(set (for [[[k1 p1] [k2 p2]] %
                                                                   :when (not= k2 k)]
                                                               [[k1 p1] [k2 p2]])))
                          (update :ext-output-coupling #(set (for [[[k1 p1] [k2 p2]] %
                                                                   :when (not= k1 k)]
                                                               [[k1 p1] [k2 p2]])))
                          (update :int-coupling        #(set (for [[[k1 p1] [k2 p2]] %
                                                                   :when (and (not= k1 k)
                                                                              (not= k2 k))]
                                                               [[k1 p1] [k2 p2]]))))
          simulators' (dissoc simulators k)
          event-list' (disj event-list [(last-time (simulators k)) k])
          next-time'  (ffirst event-list')]
      (Coord. model' last-time next-time' simulators' event-list')))
  (add-coupling [this k1 p1 k2 p2]
    (let [type   (cond
                   (= k1 ::parent) :ext-input-coupling
                   (= k2 ::parent) :ext-output-coupling
                   :else           :int-coupling)
          model' (update model type conj [[k1 p1] [k2 p2]])]
      (Coord. model' last-time next-time simulators event-list)))
  (rem-coupling [this k1 p1 k2 p2]
    (let [type   (cond
                   (= k1 ::parent) :ext-input-coupling
                   (= k2 ::parent) :ext-output-coupling
                   :else           :int-coupling)
          model' (update model type disj [[k1 p2] k2 p2])]
      (Coord. model' last-time next-time simulators event-list))))

(defn model->simulator [m]
  (case (:type m)
    ::atomic  (Sim. m 0 infinity nil)
    ::coupled (let [simulators (reduce-kv (fn [m k m']
                                            (assoc m k (model->simulator m')))
                                          {}
                                          (:components m))
                    event-list (sorted-set-by (fn [[t1 k1] [t2 k2]]
                                                (let [c (compare t1 t2)]
                                                  (if (zero? c)
                                                    ((:select m) k1 k2)
                                                    c))))]
                (Coord. m 0 infinity simulators event-list))))

(defprotocol RootCoordinator
  (start! [this])
  (event! [this v])
  (stop!  [this]))

(defrecord RootCoord [sim sim-start chan-in chan-out]
  RootCoordinator
  ;; This version does not try to catch up if sim-time falls behind
  ;; wallclock-time.
  (start! [this]
    ;; Assuming that the sim runs in real-time.
    ;; wc = wallclock
    (let [wc-start (now)]
      (go
        (loop [sim     (init sim sim-start)
               wc-last wc-start]
          (let [wc-t     (now)
                wc-delta (- wc-t wc-last)
                sim-last (last-time sim)
                sim-next (next-time sim)
                sim-t    (min (+ sim-last wc-delta) sim-next)
                wc-delta (- sim-next sim-t)
                tout     (timeout wc-delta)
                [v ch]   (alts! [tout chan-in] :priority true)]
            (condp = ch
              chan-in (if (nil? v)
                        (close! chan-out)
                        (let [wc-t     (now)
                              wc-delta (- wc-t wc-last)
                              sim-last (last-time sim)
                              sim-next (next-time sim)
                              sim-t    (min (+ sim-last wc-delta) sim-next)]
                          (recur (ext-update sim v sim-t)
                                 wc-t)))
              tout    (let [[sim' y] (int-update sim (next-time sim))]
                        (when y (>! chan-out [(next-time sim) y]))
                        (recur sim' wc-t)))))))
    nil)
  (event! [this v] (go (>! chan-in v)) nil)
  (stop!  [this]   (close! chan-in) nil))

(defn system [model start-time chan-in chan-out]
  (let [sim (model->simulator model)]
    (RootCoord. sim start-time chan-in chan-out)))
