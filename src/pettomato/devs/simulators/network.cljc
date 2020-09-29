(ns pettomato.devs.simulators.network
  (:require
   [clojure.set :refer [difference]]
   [pettomato.devs.models.network-structure :refer [network-name factor-routes route-messages]]
   [pettomato.devs.models.network :refer [network-model?]]
   [pettomato.devs.ExecSimulator :refer [get-network-structure]]
   [pettomato.devs.Simulator :refer [Simulator
                                     receive-i-message
                                     receive-*-message
                                     receive-x-message
                                     time-of-last-event
                                     time-of-next-event]]
   [pettomato.devs.simulators.executive :refer [executive-simulator]]
   [pettomato.devs.priority-queue :as pq]))

;; The executive is not included in the network models, but it may be included
;; in the network routes.

(defn execute-output [sims event-list t]
  (let [imminent       (pq/peek event-list)
        ;; This can be done in parallel.
        sim-mail-pairs (map (fn [k] (receive-*-message (get sims k) t))
                            imminent)
        new-sims       (zipmap imminent (map first  sim-mail-pairs))
        mail-out       (zipmap imminent (map second sim-mail-pairs))
        sims'          (merge sims new-sims)]
    [sims' mail-out]))

(defn execute-state-transitions [sims event-list mail t]
  (let [imminent     (if (= t (pq/peek-key event-list))
                       (pq/peek event-list)
                       #{})
        needs-update (into imminent (keys mail))
        ;; This can be done in parallel.
        updated-sims (map (fn [k]
                            (let [sim  (get sims k)
                                  mail (get mail k)]
                              ;; Mail will be empty for imminents
                              ;; not receiving mail.
                              (receive-x-message sim mail t)))
                          needs-update)
        sims'        (merge sims (zipmap needs-update updated-sims))
        event-list'  (pq/change-priority* event-list
                                          (for [k needs-update]
                                            [(time-of-next-event (get sims k))
                                             k
                                             (time-of-next-event (get sims' k))]))]
    [sims' event-list']))

(defn get-models [executive-sim]
  (:models (get-network-structure executive-sim)))

(defn get-routes [executive-sim]
  (let [network (get-network-structure executive-sim)]
    (factor-routes (:routes network))))

(defn update-network [sims event-list executive-name sim-fns t]
  (let [executive-sim (get sims executive-name)
        models        (get-models executive-sim)
        routes        (get-routes executive-sim)
        added         (difference (-> (keys models) set)
                                  (-> (keys sims) set (disj executive-name)))
        removed       (difference (-> (keys sims) set (disj executive-name))
                                  (-> (keys models) set))
        old-sims      sims
        ;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.3385&rep=rep1&type=pdf
        ;; 1. Create models.
        sims          (reduce (fn [sims model-name]
                                (let [model  (get models model-name)
                                      sim-fn (sim-fns model-name model)
                                      sim    (sim-fn model)
                                      sim    (receive-i-message sim t)]
                                  (assoc sims model-name sim)))
                              sims
                              added)
        ;; 2. Create couplings.
        ;; TODO
        ;; 3. Remove couplings.
        ;; TODO
        ;; 4. Remove models
        sims       (reduce (fn [sims model-name]
                             ;; TODO: destroy
                             (dissoc sims model-name))
                           sims
                           removed)
        event-list (reduce (fn [pq model-name]
                             (let [sim (get sims model-name)]
                               (pq/insert pq (time-of-next-event sim) model-name)))
                           event-list
                           added)
        event-list (reduce (fn [pq model-name]
                             (let [sim (get old-sims model-name)]
                               (pq/delete pq (time-of-next-event sim) model-name)))
                           event-list
                           removed)]
    [sims event-list]))

(defrecord NetworkSimulator [model sim-fns sims event-list tl tn]
  Simulator
  (receive-i-message [this t]
    #_(printf "[%s] receive-i-message\n" t)
    (let [exec-name         (:executive-name  model)
          exec-model        (:executive-model model)
          exec-sim          (executive-simulator exec-model) ;; TODO: Don't hardcode this.
          ;; The exec needs to be initialized before its children.
          exec-sim          (receive-i-message exec-sim t)
          ;; Include the exec.
          sims              (assoc sims exec-name exec-sim)
          event-list        (-> (pq/priority-queue)
                                (pq/insert (time-of-next-event exec-sim) exec-name))
          ;; Get the simulators.
          [sims event-list] (update-network sims event-list exec-name sim-fns t)
          tl                (apply max (map time-of-last-event (vals sims)))
          tn                (or (pq/peek-key event-list)
                                ;; The event-list implementation doesn't add any items
                                ;; with k = infinity.
                                (time-of-next-event exec-sim))]
      (NetworkSimulator. model sim-fns sims event-list tl tn)))
  (receive-*-message [this t]
    #_(printf "[%s] receive-*-message\n" t)
    ;; Get mail. Distribute internal mail. Return external mail.
    ;; This implementation includes receive-y-message.
    (assert (= t tn) (str "(= " t " " tn ")"))
    (let [exec-name           (:executive-name model)
          exec-sim            (get sims exec-name)
          ;; Compute output from all imminent simulators (in parallel).
          [sims' mail-out]    (execute-output sims event-list t)
          ;; Route mail. Based on original exec, if changed.
          routes              (get-routes exec-sim)
          mail-in             (route-messages routes mail-out)
          ext-mail            (get    mail-in network-name)
          int-mail            (dissoc mail-in network-name)
          ;; Execute state transitions (in parallel).
          [sims' event-list'] (execute-state-transitions sims' event-list int-mail t)
          ;; Update the network structure, if changed.
          [sims' event-list'] (update-network sims' event-list' exec-name sim-fns t)
          tl                  t
          tn                  (pq/peek-key event-list')
          sim                 (NetworkSimulator. model sim-fns sims' event-list' tl tn)]
      [sim ext-mail]))
  (receive-x-message [this x t]
    #_(printf "[%s] receive-x-message\n" t)
    (assert (<= tl t tn) (str "(<= " tl " " t " " tn ")"))
    (let [exec-name           (:executive-name model)
          exec-sim            (get sims exec-name)
          routes              (get-routes exec-sim)
          int-mail            (route-messages routes x)
          [sims' event-list'] (execute-state-transitions sims event-list int-mail t)
          ;; Possibly update network.
          [sims' event-list'] (update-network sims' event-list' exec-name sim-fns t)
          tl                  t
          tn                  (pq/peek-key event-list')
          sim                 (NetworkSimulator. model sim-fns sims' event-list' tl tn)]
      sim))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] tn))

(defn network-simulator
  "sim-fns :: model-name model -> simulator"
  [sim-fns model]
  (assert (network-model? model))
  (let [sims       {}
        event-list nil
        tl         nil
        tn         nil]
    (NetworkSimulator. model sim-fns sims event-list tl tn)))
