(ns pettomato.devs.examples.models.tms-example
  "A dynamic structure example from Theory of Modeling and Simulation,
  2nd Ed., pp. 237-240.

  This server moves workers from a static pool of workers to where the
  work is."
  (:require
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.models.atomic-model :refer [def-atomic-model time-advance]]
   [pettomato.devs.models.executive-model :refer [def-executive-model]]
   [pettomato.devs.models.network-model :refer [network-model]]))

(def ^:private server m/buffer+)

(defn- add-server [state id]
  (log/tracef "add-server: %s" id)
  (let [parent-id (:id state)]
    (update state :structure-changes conj
           [:add-model id [(server) h/zero]]
           [:connect [parent-id [:out id] id :in]]
           [:connect [id :out parent-id [:in id]]])))

(defn- rem-server [state id]
  (log/tracef "rem-server: %s" id)
  (let [parent-id (:id state)]
    (update state :structure-changes conj
            [:rem-model id]
            [:disconnect [parent-id [:out id] id :in]]
            [:disconnect [id :out parent-id [:in id]]])))

(defn- idle [state id]
  (log/tracef "idle: %s" id)
  (update state :idle conj id))

(defn- maybe-process-next [state]
  (log/trace "maybe-process-next")
  (if (and (seq (:idle state)) (seq (:queue state)))
    (let [w (first (:idle state))
          v (first (:queue state))]
      (-> state
          (update :queue rest)
          (update :idle rest)
          (update-in [:output [:out w]] conj [(:effort v) (assoc v :worker w)])))
    state))

(defn- enqueue [state v t]
  (log/tracef "enqueue: %s" v)
  (update state :queue conj (assoc v :arrival-time t)))

(defn- send-job [state v t]
  (log/tracef "send-job: %s" v)
  (update-in state [:output :out] conj (assoc v :departure-time t)))

(defn- dispatch [state [port v] t]
  (case port
    :add    (-> state
                (add-server v)
                (idle v)
                maybe-process-next)
    :remove (let [id (first (:idle state))]
              (-> state
                  (rem-server id)
                  (update :idle rest)
                  (update-in [:output :send] conj id)))
    :in     (-> state
                (enqueue v t)
                maybe-process-next)
    (case (first port)
      :in (-> state
              (send-job v t)
              (idle (second port))
              maybe-process-next))))

(def-executive-model Queue [id idle queue output structure-changes total-elapsed]
  (internal-update [state]
    ;; It is not necessary to update total-elapsed here, since the
    ;; elapsed time could only be h/zero. IS THAT WRONG? Should we
    ;; account for the minimum epsilon delta?
    (-> state
        (update :output empty)
        (update :structure-changes empty)))
  (external-update [state elapsed mail]
    (let [t          (h/+ total-elapsed elapsed)
          flat-mail  (for [[port vs] mail, v vs] [port v])
          state'     (reduce #(dispatch %1 %2 t) state flat-mail)
          queue-size (count (:queue state'))
          idle-size  (count (:idle state'))]
      (-> state'
          (update-in [:output :size] conj [queue-size idle-size])
          (assoc :total-elapsed t))))
  (output [state] output)
  (time-advance [state]
    (if (or (seq output) (seq structure-changes))
      h/zero
      h/infinity))
  (structure-changes [state] structure-changes))

(defn queue [id n-servers]
  (let [queue-size 0
        idle-size  n-servers
        server-ids (repeatedly n-servers #(gensym "worker-"))
        state      (map->Queue {:id                id
                                :idle              server-ids
                                :queue             []
                                :output            {:init [[queue-size idle-size]]}
                                :structure-changes []
                                :total-elapsed     h/zero})]
    (reduce add-server state server-ids)))

(defn node [n-servers]
  (network-model
   :queue
   [(queue :queue n-servers) h/zero]
   {}
   [[:network :in :queue :in]
    [:network :remove :queue :remove]
    [:network :add :queue :add]
    [:queue :size :network :size]
    [:queue :init :network :init]
    [:queue :send :network :send]
    [:queue :out :network :out]]))

(defn- update-size [state k q-size idle-size]
  (-> state
      (assoc-in [:queue-sizes k] q-size)
      (assoc-in [:idle-sizes  k] idle-size)))

(defn- maybe-move [state]
  ;; If there is an idle server in node-i, and (size of the queue in
  ;; node-j - number of servers in transit to node-j) > T, then move
  ;; an idle server from node-i to node-j.
  (let [threshold               (:threshold     state)
        [k1-q       k2-q      ] (:queue-sizes   state)
        [k1-idle    k2-idle   ] (:idle-sizes    state)
        [k1-transit k2-transit] (:in-transit-to state)]
    (cond
      (and (> k1-idle 0)
           (> (- k2-q k2-transit) threshold)) (-> state
                                                  (update-in [:output [:ask 0]] conj true)
                                                  (update-in [:in-transit-to 1] inc))
      (and (> k2-idle 0)
           (> (- k1-q k1-transit) threshold)) (-> state
                                                  (update-in [:output [:ask 1]] conj true)
                                                  (update-in [:in-transit-to 0] inc))
      :else                                   state)))

(def-atomic-model Control [queue-sizes idle-sizes output threshold]
  (internal-update [state]
    (update state :output empty))
  (external-update [state elapsed mail]
    (let [state (assoc state :in-transit-to [0 0])]
      (-> (reduce (fn [state [port [q-size idle-size]]]
                    (case port
                      :init1 (update-size state 0 q-size idle-size)
                      :init2 (update-size state 1 q-size idle-size)
                      :size1 (update-size state 0 q-size idle-size)
                      :size2 (update-size state 1 q-size idle-size)))
                  state
                  (for [[port vs] mail, v vs] [port v]))
          maybe-move)))
  (output [state] output)
  (time-advance [state]
    (if (seq output)
      h/zero
      h/infinity)))

(defn control [threshold]
  (map->Control {:queue-sizes [0 0]
                 :idle-sizes  [0 0]
                 :output      {}
                 :threshold   threshold}))

(defn network [n-servers-per-node threshold]
  (m/static-network-model
   {:control [(control threshold) h/zero]
    :node-1  [(node n-servers-per-node) h/zero]
    :node-2  [(node n-servers-per-node) h/zero]}
   [[:network :in1 :node-1 :in]
    [:network :in2 :node-2 :in]
    [:control [:ask 0] :node-1 :remove]
    [:control [:ask 1] :node-2 :remove]
    ;; node 1 output
    [:node-1 :size :control :size1]
    [:node-1 :init :control :init1]
    [:node-1 :send :node-2 :add]
    [:node-1 :out :network :out]
    ;; node 2 output
    [:node-2 :size :control :size2]
    [:node-2 :init :control :init2]
    [:node-2 :send :node-1 :add]
    [:node-2 :out :network :out]]))
