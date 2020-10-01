(ns pettomato.devs.simulators.coordinator
  "A simulator for a coupled model.

  Based on Ziegler, et al. Theory of Modeling and Simulation. 2nd Ed. Ch. 11.4.

  This implementation differs from the textbook to follow a functional
  programming style. Specifically, the implementation for \"receive y-message\"
  is included with \"receive-*-message\"."
  (:require
   [pettomato.devs.models.coupled :refer [coupled-model?]]
   [pettomato.devs.models.network-structure :refer [network-name factor-routes route-messages]]
   [pettomato.devs.Simulator :refer [Simulator
                                     receive-i-message
                                     receive-*-message
                                     receive-x-message
                                     time-of-last-event
                                     time-of-next-event]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [infinity]]))

(defn execute-output% [k->sim t]
  (let [ks           (keys k->sim)
        sims         (vals k->sim)
        ;; This can be done in parallel.
        xs           (map #(receive-*-message % t) sims)
        k->sim'      (zipmap ks (map first  xs))
        k->mail-from (zipmap ks (map second xs))]
    [k->sim' k->mail-from]))

(defn execute-output [k->sim queue t]
  (let [imminent-ks            (pq/peek queue)
        [k->sim' k->mail-from] (execute-output% (select-keys k->sim imminent-ks) t)]
    [(merge k->sim k->sim') k->mail-from]))

(defn execute-state-transitions [k->sim queue k->mail-to t]
  (let [imminent-ks     (if (= t (pq/peek-key queue))
                          (pq/peek queue)
                          #{})
        needs-update-ks (into imminent-ks (keys k->mail-to))
        ;; This can be done in parallel.
        updated-sims    (map (fn [k]
                               (let [sim     (get k->sim k)
                                     mail-to (get k->mail-to k)]
                                 ;; Mail will be empty for imminents
                                 ;; not receiving mail.
                                 (receive-x-message sim mail-to t)))
                             needs-update-ks)
        k->sim'         (merge k->sim (zipmap needs-update-ks updated-sims))
        queue'          (pq/change-priority* queue
                                             (for [k needs-update-ks]
                                               [(time-of-next-event (get k->sim k))
                                                k
                                                (time-of-next-event (get k->sim' k))]))]
    [k->sim' queue']))

(defrecord Coordinator [k->sim routes queue tl]
  Simulator
  (receive-i-message [this t]
    (let [k->sim (zipmap (keys k->sim) (map #(receive-i-message % t) (vals k->sim)))
          queue  (reduce-kv (fn [pq k sim] (pq/insert pq (time-of-next-event sim) k))
                            (pq/priority-queue)
                            k->sim)
          tl     (apply max (map time-of-last-event (vals k->sim)))]
      (Coordinator. k->sim routes queue tl)))
  (receive-*-message [this t]
    ;; This implementation includes receive-y-message.
    (let [tn (time-of-next-event this)]
      (assert (= t tn) (str "(= " t " " tn ")")))
    (let [;; Compute output from all imminent simulators (in parallel).
          [k->sim' k->mail-from] (execute-output k->sim queue t)
          ;; Route the mail
          k->mail-to             (route-messages routes k->mail-from)
          k->mail-to-ext         (get    k->mail-to network-name)
          k->mail-to-int         (dissoc k->mail-to network-name)
          ;; Execute state transitions (in parallel).
          [k->sim' queue']       (execute-state-transitions k->sim' queue k->mail-to-int t)
          sim                    (Coordinator. k->sim' routes queue' t)]
      [sim k->mail-to-ext]))
  (receive-x-message [this x t]
    (let [tn (time-of-next-event this)]
      (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")")))
    (let [k->mail-to       (route-messages routes x)
          ;; Execute state transitions (in parallel).
          [k->sim' queue'] (execute-state-transitions k->sim queue k->mail-to t)]
      (Coordinator. k->sim routes queue' t)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] (or (pq/peek-key queue) infinity)))

(defn coordinator
  "A simulator for a coupled model.

  sim-fns - A function that takes a model-name, k, and a network model and
  returns an appropriate simulator for that model.

  model - A coupled model."
  [sim-fns model]
  (assert (coupled-model? model))
  (let [k->sim (reduce-kv (fn [m k model]
                            (let [sim-fn (sim-fns k model)]
                              (assert sim-fn (str "No simulator specified for " k))
                              (assoc m k (sim-fn model))))
                          {}
                          (:models model))
        routes (factor-routes (:routes model))
        queue  nil
        tl     nil]
    (Coordinator. k->sim routes queue tl)))
