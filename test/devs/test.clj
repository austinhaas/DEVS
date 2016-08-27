
;;; This was an old demo of a dynamic system. It hasn't been updated
;;; to work with the current code base.
(comment
(ns pettomato.devs.test
  (:require
   [clojure.test :refer :all]
   [clojure.core.match :refer [match]]
   [clojure.core.async :as async :refer [chan go <! timeout close! >!]]
   [pettomato.lib.coll :refer [dissoc-in]]
   [pettomato.lib.number :refer [infinity]]
   [pettomato.devs.atomic-model :refer [atomic-model]]
   [pettomato.devs.executive-model :refer [executive-model]]
   [pettomato.devs.executive-network-model :refer [executive-network-model]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.executive-network-simulator :refer [network-simulator]]
   [pettomato.devs.real-time-system :refer [real-time-system]]
   [pettomato.devs.immediate-system :refer [immediate-system]]))

(defn generator [period]
  (atomic-model
   ["active" period]
   (fn [s] (let [[phase sigma] s]
             ["active" period]))
   nil
   nil
   (fn [s] (let [[phase sigma] s]
             (case phase
               "active" [['out 1]])))
   (fn [s] (let [[phase sigma] s]
             sigma))))

(defn switch [processing-time]
  (atomic-model
   ["passive" infinity 'in #{} true]
   (fn [s]
     (let [[phase sigma inport store Sw] s]
       ["passive" infinity inport store Sw]))
   (fn [s e x]
     (assert (= 1 (count x)))
     (let [[phase sigma inport store Sw] s
           [p v]                         (first x)]
       (if (= phase "passive")
         ["busy" processing-time p v (not Sw)]
         [phase (- sigma e) inport store Sw])))
   nil
   (fn [s]
     (let [[phase sigma inport store Sw] s]
       (match [phase Sw inport]
         ["busy" true  'in ] [['out  store]]
         ["busy" true  'in1] [['out1 store]]
         ["busy" false 'in ] [['out1 store]]
         ["busy" false 'in1] [['out  store]])))
   (fn [s]
     (let [[phase sigma inport store Sw] s]
       sigma))))

(defn simple-delay-component [processing-time]
  (atomic-model
   ["passive" infinity 1]
   (fn [s]
     (let [[phase sigma store] s]
       ["passive" infinity store]))
   (fn [s e x]
     (assert (= 1 (count x)))
     (let [[phase sigma store] s
           [p v]               (first x)]
       (if (= phase "passive")
         ["busy" processing-time v]
         ["passive" (- sigma e) store])))
   nil
   (fn [s]
     (let [[phase sigma store] s]
       [['out store]]))
   (fn [s]
     (let [[phase sigma store] s]
       sigma))))

;;; Example from Theory of Modeling and Simulation, 2nd Ed., pp. 237-240.

(def server simple-delay-component)

(defn queue [k s*]
  (letfn [(add-server [s k']
            (-> s
                (assoc-in [:components k'] (server 1000))
                (assoc-in [:connections k ['out k'] k'] 'in)
                (assoc-in [:connections k' 'out k] ['in k'])))
          (rem-server [s k']
            (-> s
                (dissoc-in [:components k'])
                (dissoc-in [:connections k ['out k'] k'])
                (dissoc-in [:connections k' 'out k])))
          (idle [s k']
            (update s :idle conj k'))
          (maybe-process-next [s]
            (if (and (seq (:idle s)) (seq (:Q s)))
              (-> s
                  (update :Q rest)
                  (update :idle rest)
                  (update :output conj [['out (first (:idle s))] (first (:Q s))]))
              s))
          (enqueue [s v]
            (update s :Q conj v))
          (send [s v]
            (update s :output conj ['out v]))
          (dispatch [s ev]
            (let [[port v] ev]
              (case port
                add    (-> s (add-server v) (idle v) maybe-process-next)
                remove (-> s (rem-server (first (:idle s))) (update :idle rest)
                           (update :output conj ['send (first (:idle s))]))
                in     (-> s (enqueue v) maybe-process-next)
                (case (first port)
                  in (-> s (send v) (idle (second port)) maybe-process-next)))))]
    (executive-model
     ;; Initial state.
     (let [Q []
           S {:idle s* :Q Q :sigma 0 :output [['init [(count Q) (count s*)]]]
              :components  {}
              :connections {:N {'in     {k  'in}
                                'remove {k  'remove}
                                'add    {k  'add}}
                            k  {'size   {:N 'size}
                                'init   {:N 'init}
                                'send   {:N 'send}
                                'out    {:N 'out}}}}]
       (reduce add-server S s*))
     (fn int-update [s]
       (assoc s :sigma infinity :output []))
     (fn ext-update [s e x]
       (let [s' (-> (reduce dispatch s x)
                    ;; Assuming every external event results in an output message.
                    (assoc :sigma 0))]
         (update s' :output conj ['size [(count (:Q s')) (count (:idle s'))]])))
     nil
     :output
     :sigma)))

(defn node [servers]
  (executive-network-model :queue (queue :queue servers)))

(defn control [threshold]
  (letfn [(update-size [s k q-size idle-size]
            (-> s
                (assoc-in [:queue-sizes k] q-size)
                (assoc-in [:idle-sizes  k] idle-size)))
          (maybe-move [s]
            ;; If there is an idle server in node-i, and (size of the
            ;; queue in node-j - number of servers in transit to
            ;; node-j) > T, then move an idle server from node-i to
            ;; node-j.
            (let [[k1-q       k2-q      ] (:queue-sizes   s)
                  [k1-idle    k2-idle   ] (:idle-sizes    s)
                  [k1-transit k2-transit] (:in-transit-to s)]
              (cond
                (and (> k1-idle 0) (> (- k2-q k2-transit) threshold)) (-> s
                                                                          (update :output conj [['ask 0] nil])
                                                                          (update-in [:in-transit-to 1] inc))
                (and (> k2-idle 0) (> (- k1-q k1-transit) threshold)) (-> s
                                                                          (update :output conj [['ask 1] nil])
                                                                          (update-in [:in-transit-to 0] inc))
                :else s)))]
    (atomic-model
     {:output      []
      :sigma       infinity
      :queue-sizes [0 0]
      :idle-sizes  [0 0]}
     (fn int-update [s]
       (assoc s :sigma infinity :output []))
     (fn ext-update [s e x]
       (let [s (assoc s :in-transit-to [0 0])]
         (-> (reduce (fn [s ev]
                       (let [[port [q-size idle-size]] ev]
                         (case port
                           init1 (update-size s 0 q-size idle-size)
                           init2 (update-size s 1 q-size idle-size)
                           size1 (update-size s 0 q-size idle-size)
                           size2 (update-size s 1 q-size idle-size))))
                     (assoc s :sigma 0)
                     x)
             maybe-move)))
     nil
     :output
     :sigma)))

(def network-1
  (executive-network-model
   :network-1
   (executive-model
    {:components  {:control (control 5)
                   :node-1  (node [1 2])
                   :node-2  (node [3 4])}
     :connections {:N       {'in1     {:node-1  'in}
                             'in2     {:node-2  'in}}
                   :control {['ask 0] {:node-1  'remove}
                             ['ask 1] {:node-2  'remove}}
                   :node-1  {'size    {:control 'size1}
                             'init    {:control 'init1}
                             'send    {:node-2  'add}
                             'out     {:N       'out}}
                   :node-2  {'size    {:control 'size2}
                             'init    {:control 'init2}
                             'send    {:node-1  'add}
                             'out     {:N       'out}}}}
    nil nil nil
    nil (constantly infinity))))

;;---
#_
(do
  (def sim (network-simulator network-1))

  (def chan-in  (chan 100))
  (def chan-out (chan 100))

  (real-time-system sim 0 chan-in chan-out)

  (go (loop []
        (if-let [v (<! chan-out)]
          (do (println (format "[%s] %s" (first v) (second v)))
              (recur))
          (println 'done))))

  (go (dotimes [i 10]
        (>! chan-in [['in1 i]]))))

#_
(go
  (dotimes [i 10]
    (>! chan-in [['in1 i]])))

#_(close! chan-in)

#_
(immediate-system
 (network-simulator network-1)
 0
 infinity
 (for [i (range 10)] [1 ['in1 i]]))
)
