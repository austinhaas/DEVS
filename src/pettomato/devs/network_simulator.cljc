(ns pettomato.devs.network-simulator
  (:require
   [clojure.spec.alpha :as s]
   [pettomato.devs.models :as m]
   [pettomato.devs.Simulator :refer [Simulator
                                     receive-i-message
                                     receive-*-message
                                     receive-x-message
                                     time-of-last-event
                                     time-of-next-event]]
   [pettomato.devs.priority-queue :as pq]))

(defn factor-routes
  "Convert the flat, human-readable expression of routes to a nested map, which
  indexes the components for fast access.

  [[k1 p1 k2 p2 f]
   [k1 p3 k3 p4 f]]  -> {k1 {p1 {k2 {p2 f}}}
                             p3 {k3 {p4 f}}}"
  [routes]
  (reduce (fn [m [k1 p1 k2 p2 f]]
            (assert (not= k1 k2) "Direct feedback loops are not allowed.") ;; TMS2000 p. 86.
            (assoc-in m [k1 p1 k2 p2] (or f identity)))
          {}
          routes))

(defn route-messages
  "Returns receiver->port->vs."
  [routes sender->port->vs]
  (let [flattened (for [[sender port->vs]     sender->port->vs
                        [out-port vs]         port->vs
                        [receiver in-port->f] (get-in routes [sender out-port])
                        [in-port f]           in-port->f]
                    [sender out-port receiver in-port f vs])]
    ;; TODO: If several receivers apply the same function to the same output
    ;; port, then it might be more efficient to group by [out-port f] and
    ;; ensure that we only apply the function once.
    (reduce (fn [m [sender out-port receiver in-port f vs]]
              (update-in m [receiver in-port] into (map f) vs))
            {}
            flattened)))

(defrecord NetworkSimulator [sims routes event-list tl tn]
  Simulator
  (receive-i-message [this t]
    (let [sims       (zipmap (keys sims) (map #(receive-i-message % t) (vals sims)))
          event-list (reduce-kv (fn [pq k sim] (pq/insert pq (time-of-next-event sim) k))
                                (pq/priority-queue)
                                sims)
          tl         (apply max-key time-of-last-event (vals sims))
          tn         (pq/peek-key event-list)]
      (NetworkSimulator. sims routes event-list tl tn)))
  (receive-*-message [this t]
    ;; also includes receive-y-message
    (assert (= t tn) (str "(= " t " " tn ")"))
    (let [imminent       (pq/peek event-list)
          ;; Compute output from all imminent simulators (in parallel).
          sim-mail-pairs (map (fn [k] (receive-*-message (get sims k) t))
                              imminent)
          new-sims       (zipmap imminent (map first  sim-mail-pairs))
          mail-out       (zipmap imminent (map second sim-mail-pairs))
          sims           (merge sims new-sims)
          mail-in        (route-messages routes mail-out)
          ext-mail       (get    mail-in m/network-name)
          int-mail       (dissoc mail-in m/network-name)
          needs-update   (into imminent (keys int-mail))
          ;; Execute state transitions (in parallel).
          updated-sims   (map (fn [k]
                                (let [sim  (get sims k)
                                      mail (get int-mail k)]
                                  ;; Mail will be empty for imminents
                                  ;; not receiving mail.
                                  (receive-x-message sim mail t)))
                              needs-update)
          sims'          (merge sims (zipmap needs-update updated-sims))
          event-list     (pq/change-priority* event-list
                                              (for [k needs-update]
                                                [(time-of-next-event (get sims k))
                                                 k
                                                 (time-of-next-event (get sims' k))]))
          tl             t
          tn             (pq/peek-key event-list)
          sim            (NetworkSimulator. sims' routes event-list tl tn)]
      [sim ext-mail]))
  (receive-x-message [this x t]
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [imminent     (if (= t (pq/peek-key event-list))
                         (pq/peek event-list)
                         #{})
          int-mail     (route-messages routes x)
          needs-update (into imminent (keys int-mail))
          ;; Execute state transitions (in parallel).
          updated-sims (map (fn [k]
                              (let [sim  (get sims k)
                                    mail (get int-mail k)]
                                ;; Mail will be empty for imminents
                                ;; not receiving mail.
                                (receive-x-message sim mail t)))
                            needs-update)
          sims'        (merge sims (zipmap needs-update updated-sims))
          event-list   (pq/change-priority* event-list
                                            (for [k needs-update]
                                              [(time-of-next-event (get sims k))
                                               k
                                               (time-of-next-event (get sims' k))]))
          tl           t
          tn           (pq/peek-key event-list)
          sim          (NetworkSimulator. sims routes event-list tl tn)]
      sim))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn))

(defn network-simulator
  [{:keys [models routes simulators] :as model}]
  (assert (s/valid? ::m/network-model model))
  (let [sims   (reduce-kv (fn [m model-name model]
                            (let [sim-fn (get simulators model-name)]
                              (assert sim-fn (str "No simulator specified for " model-name))
                              (assoc m model-name (sim-fn model))))
                          {}
                          models)
        routes (factor-routes routes)]
    (NetworkSimulator. sims routes nil nil nil)))
