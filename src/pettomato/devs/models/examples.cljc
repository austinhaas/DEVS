(ns pettomato.devs.models.examples
  (:require
   [pettomato.devs.models.atomic :refer [atomic-model]]
   [pettomato.devs.models.coupled :refer [coupled-model]]
   [pettomato.devs.models.executive :refer [executive-model]]
   [pettomato.devs.models.network :refer [network-model]]
   [pettomato.devs.models.network-structure :refer [network-name]]
   [pettomato.devs.util :refer [infinity dissoc-in]]))

(defn generator
  "A model that periodically emits value on a port labeled :out."
  [value period]
  (atomic-model
   (let [s nil
         e 0]
     [s e])
   identity
   nil
   nil
   (constantly {:out [value]})
   (constantly period)))

(defn lazy-seq-generator
  "A model that emits values according to a (possibly lazy and infinite) seq
  of [sigma mail]."
  [s]
  (atomic-model
   (let [s s
         e 0]
     [s e])
   next
   nil
   nil
   (comp second first)
   (fn time-advance [s]
     (if (seq s)
       (ffirst s)
       infinity))))

(defn delay-component
  "A model takes takes a value on port :in and after processing-time, emits the
  same value on port :out.

  Ignores input while processing. If multiple inputs arrive at the same time,
  one is chosen at random."
  [processing-time]
  (atomic-model
   (let [s ["passive" infinity 1]
         e 0]
     [s e])
   (fn [s]
     (let [[phase sigma store] s]
       ["passive" infinity store]))
   (fn [s e x]
     (let [[phase sigma store] s
           v                   (rand-nth (:in x))]
       (if (= phase "passive")
         ["busy" processing-time v]
         ["passive" (- sigma e) store])))
   nil
   (fn [s]
     (let [[phase sigma store] s]
       {:out [store]}))
   (fn [s]
     (let [[phase sigma store] s]
       sigma))))
#_
(defn delay-1 [processing-time]
  (let [int-update (fn [s]
                     (assoc s :phase :passive :sigma infinity))
        ext-update (fn [s e x]
                     (assert (= 1 (count x)))
                     (let [[port v] (first x)]
                       (case (:phase s)
                         :passive (assoc s :phase :busy :sigma processing-time :store v)
                         :busy    (update s :sigma - e))))]
   (atomic-model
    {:phase :passive
     :sigma infinity
     :store nil}
    int-update
    ext-update
    (fn con-update [s e x]
      (ext-update (int-update s) 0 x))
    (fn output [s] [[:out (:store s)]])
    :sigma)))

;; Same as above, but the confluent fn prioritizes ext-update over int-update.
#_
(defn delay-2 [processing-time]
  (let [int-update (fn [s]
                     (assoc s :phase :passive :sigma infinity))
        ext-update (fn [s e x]
                     (assert (= 1 (count x)))
                     (let [[port v] (first x)]
                       (case (:phase s)
                         :passive (assoc s :phase :busy :sigma processing-time :store v)
                         :busy    (update s :sigma - e))))]
   (atomic-model
    {:phase :passive
     :sigma infinity
     :store nil}
    int-update
    ext-update
    (fn con-update [s e x]
      (int-update (ext-update s e x)))
    (fn output [s] [[:out (:store s)]])
    :sigma)))

;;;-----------------------------------------------------------------------------
;;; Example from Theory of Modeling and Simulation, 2nd Ed., pp. 237-240.

(def server delay-component)

(defn queue [k server-ks]
  (letfn [(add-server [s k']
            (-> s
                (assoc-in [:network :models k'] (server 1000))
                (update-in [:network :routes] conj
                           [k ['out k'] k' 'in]
                           [k' 'out k ['in k']])))
          (rem-server [s k']
            (-> s
                (dissoc-in [:network :models k'])
                (update-in [:network :routes] disj
                        [k ['out k'] k' 'in]
                        [k' 'out k ['in k']])))
          (idle [s k']
            (update s :idle conj k'))
          (maybe-process-next [s]
            (if (and (seq (:idle s)) (seq (:queue s)))
              (-> s
                  (update :queue rest)
                  (update :idle rest)
                  (update-in [:output ['out (first (:idle s))]] conj (first (:queue s))))
              s))
          (enqueue [s v]
            (update s :queue conj v))
          (send [s v]
            (update-in s [:output 'out] conj v))
          (dispatch [s ev]
            (let [[port v] ev]
              (case port
                add    (-> s (add-server v) (idle v) maybe-process-next)
                remove (-> s (rem-server (first (:idle s))) (update :idle rest)
                           (update-in [:output 'send] conj (first (:idle s))))
                in     (-> s (enqueue v) maybe-process-next)
                (case (first port)
                  in (-> s (send v) (idle (second port)) maybe-process-next)))))]
    (executive-model
     ;; Initial state.
     (let [queue   []
           network {:models {}
                    :routes [[network-name 'in k 'in]
                             [network-name 'remove k 'remove]
                             [network-name 'add k 'add]
                             [k 'size network-name 'size]
                             [k 'init network-name 'init]
                             [k 'send network-name 'send]
                             [k 'out network-name 'out]]}
           state   {:network network
                    :idle    server-ks
                    :queue   queue
                    :sigma   0
                    :output  {'init [[(count queue) (count server-ks)]]}}
           state   (reduce add-server state server-ks)
           e       0]
       [state e])
     (fn int-update [s]
       (assoc s :sigma infinity :output {}))
     (fn ext-update [s e x]
       (let [s' (-> (reduce dispatch s (for [[k vs] x, v vs] [k v]))
                    ;; Assuming every external event results in an output message.
                    (assoc :sigma 0))]
         (update-in s' [:output 'size] conj [(count (:queue s')) (count (:idle s'))])))
     nil
     :output
     :sigma
     :network)))

(defn node [servers]
  (network-model :queue (queue :queue servers)))

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
                                                            (update-in [:output ['ask 0]] conj nil)
                                                            (update-in [:in-transit-to 1] inc))
                (and (> k2-idle 0)
                     (> (- k1-q k1-transit) threshold)) (-> s
                                                            (update-in [:output ['ask 1]] conj nil)
                                                            (update-in [:in-transit-to 0] inc))
                :else                                   s)))]
    (atomic-model
     (let [s {:output      {}
              :sigma       infinity
              :queue-sizes [0 0]
              :idle-sizes  [0 0]}
           e 0]
       [s e])
     (fn int-update [s]
       (assoc s :sigma infinity :output {}))
     (fn ext-update [s e x]
       (let [s (assoc s :in-transit-to [0 0])]
         (-> (reduce (fn [s [port [q-size idle-size]]]
                       (case port
                         init1 (update-size s 0 q-size idle-size)
                         init2 (update-size s 1 q-size idle-size)
                         size1 (update-size s 0 q-size idle-size)
                         size2 (update-size s 1 q-size idle-size)))
                     (assoc s :sigma 0)
                     (for [[k vs] x, v vs] [k v]))
             maybe-move)))
     nil
     :output
     :sigma)))

(def network-1
  (let [models {:control (control 5)
                :node-1  (node [1 2])
                :node-2  (node [3 4])}
        routes [[network-name 'in1 :node-1 'in]
                [network-name 'in2 :node-2 'in]
                [:control ['ask 0] :node-1 'remove]
                [:control ['ask 1] :node-2 'remove]
                ;; node 1 output
                [:node-1 'size :control 'size1]
                [:node-1 'init :control 'init1]
                [:node-1 'send :node-2 'add]
                [:node-1 'out network-name 'out]
                ;; node 2 output
                [:node-2 'size :control 'size2]
                [:node-2 'init :control 'init2]
                [:node-2 'send :node-1 'add]
                [:node-2 'out network-name 'out]]]
   (coupled-model models routes)))

;;;-----------------------------------------------------------------------------
