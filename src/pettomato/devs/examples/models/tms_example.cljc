(ns pettomato.devs.examples.models.tms-example
  "A dynamic structure example from Theory of Modeling and Simulation, 2nd Ed., pp. 237-240.

  This server moves workers to where the work is, but it doesn't scale up or
  down otherwise."
  (:require
   [pettomato.devs.examples.models :refer [variable-delay]]
   [pettomato.devs.lib.hyperreal :as h :refer [H]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.vars :refer [*sim-time*]]))

(def server variable-delay)

(defn queue [k n-servers]
  (letfn [(add-server [s k']
            (log/tracef "add-server: %s" k')
            (update-in s [:output :petition] (fnil conj [])
                       [:add-model k' (server)]
                       [:connect [k [:out k'] k' :in]]
                       [:connect [k' :out k [:in k']]]))
          (rem-server [s k']
            (log/tracef "rem-server: %s" k')
            (update-in s [:output :petition] (fnil conj [])
                       [:rem-model k']
                       [:disconnect [k [:out k'] k' :in]]
                       [:disconnect [k' :out k [:in k']]]))
          (idle [s k']
            (log/tracef "idle: %s" k')
            (update s :idle conj k'))
          (maybe-process-next [s]
            (log/trace "maybe-process-next")
            (if (and (seq (:idle s)) (seq (:queue s)))
              (let [w (first (:idle s))
                    v (first (:queue s))]
                (-> s
                    (update :queue rest)
                    (update :idle rest)
                    (update-in [:output [:out w]] conj [(:effort v) (assoc v :worker w)])))
              s))
          (enqueue [s v]
            (log/tracef "enqueue: %s" v)
            (update s :queue conj (assoc v :arrival-time *sim-time*)))
          (send [s v]
            (log/tracef "send: %s" v)
            (update-in s [:output :out] conj (assoc v :departure-time *sim-time*)))
          (dispatch [s ev]
            (let [[port v] ev]
              (case port
                :add    (-> s (add-server v) (idle v) maybe-process-next)
                :remove (-> s (rem-server (first (:idle s))) (update :idle rest)
                            (update-in [:output :send] conj (first (:idle s))))
                :in     (-> s (enqueue v) maybe-process-next)
                (case (first port)
                  :in (-> s (send v) (idle (second port)) maybe-process-next)))))]
    (let [server-ks (for [i (range n-servers)] (keyword (str "worker-" (gensym))))]
      (atomic-model
       :initial-state   (let [queue []
                              state {:idle   server-ks
                                     :queue  queue
                                     :sigma  h/epsilon
                                     :output {:init [[(count queue) (count server-ks)]]}}]
                          (reduce add-server state server-ks))
       :internal-update (fn [s]
                          (log/trace "int-update")
                          (assoc s :sigma h/infinity :output {}))
       :external-update (fn [s e x]
                          (log/tracef "ext-update: %s" x)
                          (let [s' (-> (reduce dispatch s (for [[k vs] x, v vs] [k v]))
                                       ;; Assuming every external event results in an output message.
                                       (assoc :sigma h/epsilon))]
                            (update-in s' [:output :size] conj [(count (:queue s')) (count (:idle s'))])))
       :output          :output
       :time-advance    :sigma))))

(defn node [n-servers]
  (network-model {:queue (queue :queue n-servers)}
                 [[:network :in :queue :in]
                  [:network :remove :queue :remove]
                  [:network :add :queue :add]
                  [:queue :size :network :size]
                  [:queue :init :network :init]
                  [:queue :send :network :send]
                  [:queue :out :network :out]
                  [:queue :petition :network :petition]]))

(defn control [threshold]
  (letfn [(update-size [s k q-size idle-size]
            (-> s
                (assoc-in [:queue-sizes k] q-size)
                (assoc-in [:idle-sizes  k] idle-size)))
          (maybe-move [s]
            ;; If there is an idle server in node-i, and (size of the queue in
            ;; node-j - number of servers in transit to node-j) > T, then move
            ;; an idle server from node-i to node-j.
            (let [[k1-q       k2-q      ] (:queue-sizes   s)
                  [k1-idle    k2-idle   ] (:idle-sizes    s)
                  [k1-transit k2-transit] (:in-transit-to s)]
              (cond
                (and (> k1-idle 0)
                     (> (- k2-q k2-transit) threshold)) (-> s
                                                            (update-in [:output [:ask 0]] conj true)
                                                            (update-in [:in-transit-to 1] inc))
                (and (> k2-idle 0)
                     (> (- k1-q k1-transit) threshold)) (-> s
                                                            (update-in [:output [:ask 1]] conj true)
                                                            (update-in [:in-transit-to 0] inc))
                :else                                   s)))]
    (atomic-model
     :initial-state   {:output      {}
                       :sigma       h/infinity
                       :queue-sizes [0 0]
                       :idle-sizes  [0 0]}
     :internal-update (fn [s]
                        (assoc s :sigma h/infinity :output {}))
     :external-update (fn [s e x]
                        (let [s (assoc s :in-transit-to [0 0])]
                          (-> (reduce (fn [s [port [q-size idle-size]]]
                                        (case port
                                          :init1 (update-size s 0 q-size idle-size)
                                          :init2 (update-size s 1 q-size idle-size)
                                          :size1 (update-size s 0 q-size idle-size)
                                          :size2 (update-size s 1 q-size idle-size)))
                                      (assoc s :sigma h/epsilon)
                                      (for [[k vs] x, v vs] [k v]))
                              maybe-move)))
     :output          :output
     :time-advance    :sigma)))

(defn network-1 [n-servers-per-node threshold]
  (network-model
   {:control (control threshold)
    :node-1  (node n-servers-per-node)
    :node-2  (node n-servers-per-node)}
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
