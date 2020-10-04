(ns pettomato.devs.simulators.network
  "A network simulator, loosely based on DSDE.

  Barros. Abstract Simulators for the DSDE Formalism. 1998.
  https://repository.lib.ncsu.edu/bitstream/handle/1840.4/6989/1998_0056.pdf

  The executive is not included in the network models, but it may be included
  in the network routes.

  This is not an optimal implementation. In particular, the executive returns
  network structures, and the simulator has to find the delta in order to make
  the appropriate updates. It would be more efficient if the executive returned
  the delta. It was originally implemented this way to resemble the math in the
  literature."
  (:require
   [clojure.set :refer [difference]]
   [pettomato.devs.ExecSimulator :refer [get-network-structure]]
   [pettomato.devs.Simulator :refer [Simulator
                                     receive-i-message
                                     receive-*-message
                                     receive-x-message
                                     time-of-last-event
                                     time-of-next-event]]
   [pettomato.devs.models.network :refer [network-model?]]
   [pettomato.devs.models.network-structure :refer [network-name factor-routes route-messages]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.simulators.coordinator :refer [execute-output execute-state-transitions]]
   [pettomato.devs.util :refer [infinity trace]]))

(defn update-network [sim-fns exec-name k->sim queue t]
  ;; Compare the old network to the new network to determine what has
  ;; changed. This is inefficient, but for now, it is logical and it resembles
  ;; the math in the literature.
  (let [exec-sim        (get k->sim exec-name)
        network         (get-network-structure exec-sim)
        k->model        (:models network)
        routes          (factor-routes (:routes network))
        model-ks        (set (keys k->model))
        sim-ks          (set (keys k->sim))
        ;; The exec isn't included in the network models, so it isn't part of
        ;;the comparison. Also, the exec cannot be removed.
        non-exec-sim-ks (disj sim-ks exec-name)
        added-ks        (difference model-ks non-exec-sim-ks)
        removed-ks      (difference non-exec-sim-ks model-ks)

        ;; Himmelspach, Jan, and Adelinde M. Uhrmacher. "Processing dynamic PDEVS models." The IEEE Computer Society's 12th Annual International Symposium on Modeling, Analysis, and Simulation of Computer and Telecommunications Systems, 2004.(MASCOTS 2004). Proceedings.. IEEE, 2004.
        ;; Section 3.2
        ;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.3385&rep=rep1&type=pdf

        ;; This order is significant for correctness. The numbers below follow
        ;; section 3.2 in the paper above. The x and y were added to update the
        ;; queue.

        ;; 1. Create models.
        k->sim          (reduce (fn [k->sim k]
                                  (let [model  (get k->model k)
                                        sim-fn (sim-fns k model)
                                        sim    (sim-fn model)
                                        sim    (try (receive-i-message sim t)
                                                    (catch Exception e
                                                      (throw (ex-info "Error in model during recieve-i-message"
                                                                      {:k k}
                                                                      e))))]
                                    (assoc k->sim k sim)))
                                k->sim
                                added-ks)
        ;; x. Add sims to the queue.
        queue           (reduce (fn [pq k]
                                  (let [sim (get k->sim k)
                                        tn  (time-of-next-event sim)]
                                    (pq/insert pq tn k)))
                                queue
                                added-ks)
        ;; 2. Create couplings.
        ;;    Not necessary in current implementation; couplings are replaced entirely.
        ;; 3. Remove couplings.
        ;;    Not necessary in current implementation; couplings are replaced entirely.
        ;; y. Remove sims from the queue.
        queue           (reduce (fn [pq k]
                                  (let [sim (get k->sim k)
                                        tn  (time-of-next-event sim)]
                                    (pq/delete pq tn k)))
                                queue
                                removed-ks)
        ;; 4. Remove models.
        k->sim          (reduce (fn [k->sim k]
                                  ;; TODO: destroy
                                  (dissoc k->sim k))
                                k->sim
                                removed-ks)]
    [k->sim routes queue]))

(defrecord NetworkSimulator [model sim-fns k->sim routes queue tl]
  Simulator
  (receive-i-message [this t]
    (trace "[%s] ************************************" t)
    (trace "[%s] NetworkSimulator/receive-i-message" t)
    (let [exec-name             (:executive-name  model)
          exec-model            (:executive-model model)
          sim-fn                (sim-fns exec-name exec-model)
          exec-sim              (sim-fn exec-model)
          ;; The exec needs to be initialized before its children.
          exec-sim              (try (receive-i-message exec-sim t)
                                     (catch Exception e
                                       (throw (ex-info "Error in model during recieve-i-message"
                                                       {:k exec-name}
                                                       e))))
          ;; Include the exec.
          k->sim                (assoc k->sim exec-name exec-sim)
          ;; Create a new queue. Seed with exec.
          queue                 (pq/priority-queue (time-of-next-event exec-sim) exec-name)
          ;; Initialize the network.
          [k->sim routes queue] (update-network sim-fns exec-name k->sim queue t)
          tl                    (apply max (map time-of-last-event (vals k->sim)))]
      (NetworkSimulator. model sim-fns k->sim routes queue tl)))
  (receive-*-message [this t]
    (trace "[%s] ====================================" t)
    (trace "[%s] NetworkSimulator/receive-*-message" t)
    ;; Get mail. Distribute internal mail. Return external mail.
    ;; This implementation includes receive-y-message.
    (let [tn (time-of-next-event this)]
      (when-not (= t tn)
        (throw (ex-info (str "synchronization error" " (= " t " " tn ")")
                        {:t  t
                         :tn tn
                         :tl tl}))))
    (let [;; Compute output from all imminent simulators (in parallel).
          [k->sim' k->mail-from]   (execute-output k->sim queue t)
          ;; Route the mail.
          k->mail-to               (route-messages routes k->mail-from)
          _                        (trace "[%s]   k->mail-to: %s" t k->mail-to)
          k->mail-to-ext           (get    k->mail-to network-name)
          k->mail-to-int           (dissoc k->mail-to network-name)
          ;; Execute state transitions (in parallel).
          [k->sim' queue']         (execute-state-transitions k->sim' queue k->mail-to-int t)
          ;; Update the network structure, if changed.
          exec-name                (:executive-name model)
          [k->sim' routes' queue'] (update-network sim-fns exec-name k->sim' queue' t)
          sim                      (NetworkSimulator. model sim-fns k->sim' routes' queue' t)]
      [sim k->mail-to-ext]))
  (receive-x-message [this x t]
    (trace "[%s] ====================================" t)
    (trace "[%s] NetworkSimulator/receive-x-message: %s" t x)
    (let [tn (time-of-next-event this)]
      (when-not (<= tl t tn)
        (throw (ex-info (str "synchronization error" " (<= " tl " " t " " tn ")")
                        {:t  t
                         :tn tn
                         :tl tl}))))
    (let [k->mail-to               (route-messages routes x)
          _                        (trace "[%s]   k->mail-to: %s" t k->mail-to)
          ;; Execute state transitions (in parallel).
          [k->sim' queue']         (execute-state-transitions k->sim queue k->mail-to t)
          ;; Update the network structure, if changed.
          exec-name                (:executive-name model)
          [k->sim' routes' queue'] (update-network sim-fns exec-name k->sim' queue' t)]
      (NetworkSimulator. model sim-fns k->sim' routes' queue' t)))
  (time-of-last-event [this] tl)
  (time-of-next-event [this] (or (pq/peek-key queue) infinity)))

(defn network-simulator
  "A simulator for a network model.

  sim-fns - A function that takes a model-name, k, and a network model and
  returns an appropriate simulator for that model.

  model - A network model."
  [sim-fns model]
  (assert (network-model? model))
  (let [k->sim {}
        routes []
        queue  nil
        tl     nil]
    (NetworkSimulator. model sim-fns k->sim routes queue tl)))
