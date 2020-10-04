(ns pettomato.devs.dsdevs-test
  #_
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.afap-root-simulator :as root-sim :refer [afap-root-simulator]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.models :refer [atomic-model executive-model network-model network-id register unregister connect disconnect]]
   [pettomato.devs.network-simulator :refer [network-simulator]]
   [pettomato.devs.test-util :refer [eq?]]
   [pettomato.devs.util :refer [infinity]])
  )

;;; Example from Theory of Modeling and Simulation, 2nd Ed., pp. 237-240.

(def server simple-delay-component)

(defn queue [k s*]
  (letfn [(add-server [s k']
            (-> s
                (register k' (server 1000))
                (connect k ['out k'] k' 'in)
                (connect k' 'out k ['in k'])))
          (rem-server [s k']
            (-> s
                (unregister k')
                (disconnect k ['out k'] k' 'in)
                (disconnect k' 'out k ['in k'])))
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
           S (-> {:idle s* :Q Q :sigma 0 :output [['init [(count Q) (count s*)]]]}
                 (connect network-id 'in k 'in)
                 (connect network-id 'remove k 'remove)
                 (connect network-id 'add k 'add)
                 (connect k 'size network-id 'size)
                 (connect k 'init network-id 'init)
                 (connect k 'send network-id 'send)
                 (connect k 'out network-id 'out))]
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
  (network-model :queue (queue :queue servers)))

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
         (-> (reduce (fn [s [port [q-size idle-size]]]
                       (case port
                         init1 (update-size s 0 q-size idle-size)
                         init2 (update-size s 1 q-size idle-size)
                         size1 (update-size s 0 q-size idle-size)
                         size2 (update-size s 1 q-size idle-size)))
                     (assoc s :sigma 0)
                     x)
             maybe-move)))
     nil
     :output
     :sigma)))

(def network-1
  (network-model
   :network-1
   (executive-model
    (-> {}
        (register :control (control 5))
        (register :node-1 (node [1 2]))
        (register :node-2 (node [3 4]))
        (connect network-id 'in1 :node-1 'in)
        (connect network-id 'in2 :node-2 'in)
        (connect :control ['ask 0] :node-1 'remove)
        (connect :control ['ask 1] :node-2 'remove)
        (connect :node-1 'size :control 'size1)
        (connect :node-1 'init :control 'init1)
        (connect :node-1 'send :node-2 'add)
        (connect :node-1 'out network-id 'out)
        (connect :node-2 'size :control 'size2)
        (connect :node-2 'init :control 'init2)
        (connect :node-2 'send :node-1 'add)
        (connect :node-2 'out network-id 'out))
    nil nil nil
    nil (constantly infinity))))
#_
(deftest dynamic-network-test
  (is ((fn [ev*]
         ;; This is ugly. The idea is that there should only be two
         ;; events, and they each output 5 jobs, but which jobs is not
         ;; important.
         (and (= (count ev*) 2)
              (= (first (first ev*)) 1001)
              (= (first (second ev*)) 2001)
              (= (count (second (first ev*))) 5)
              (= (count (second (second ev*))) 5)))
       (-> network-1
           network-simulator
           (afap-root-simulator 0 infinity [[1 [['in1 0]
                                                ['in1 1]
                                                ['in1 2]
                                                ['in1 3]
                                                ['in1 4]
                                                ['in1 5]
                                                ['in1 6]
                                                ['in1 7]
                                                ['in1 8]
                                                ['in1 9]]]])
           second))))
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
#_
(deftest confluence-test
  (is (eq? (-> (delay-1 10)
               atomic-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]
            [20 [[:out 2]]]]))

  ;; The second input value should be ignored, since the model is in
  ;; the busy state and external inputs have higher priority in
  ;; delay-2.
  (is (eq? (-> (delay-2 10)
               atomic-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]]))

  (is (eq? (-> (network-model
                :exec
                (executive-model
                 (-> {}
                     (register :delay (delay-1 10))
                     (connect network-id :in :delay :in)
                     (connect :delay :out network-id :out))
                 nil nil nil nil (constantly infinity)))
               network-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]
            [20 [[:out 2]]]]))

  (is (eq? (-> (network-model
                :exec
                (executive-model
                 (-> {}
                     (register :delay (delay-2 10))
                     (connect network-id :in :delay :in)
                     (connect :delay :out network-id :out))
                 nil nil nil nil (constantly infinity)))
               network-simulator
               (afap-root-simulator 0 100 [[0  [[:in 1]]]
                                           [10 [[:in 2]]]])
               second)
           [[10 [[:out 1]]]])))
