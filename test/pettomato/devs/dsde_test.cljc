(ns pettomato.devs.dsde-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.lib.random :as rand]
   [pettomato.devs.dsde :refer :all]
   [pettomato.devs.models.network-structure :refer [network-name
                                                    empty-network
                                                    add-model
                                                    rem-model
                                                    connect
                                                    disconnect]]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.queue :refer [queue]]))

;;; Test models

(defrecord generator [value period]
  IModel
  (initial-total-state [model]
    (let [s nil
          e 0]
      [s e]))
  (internal-update     [model state] state)
  (external-update     [model state elapsed messages] state)
  (confluent-update    [model state messages] state)
  (partial-output      [model state] {:out [value]})
  (time-advance        [model state] period))

(defrecord lazy-seq-generator [s]
  IModel
  (initial-total-state [model]
    (let [s s
          e 0]
      [s e]))
  (internal-update     [model state] (next state))
  (external-update     [model state elapsed messages] state)
  (confluent-update    [model state messages] state)
  (partial-output      [model state] (second (first state)))
  (time-advance        [model state] (if (seq state)
                                       (ffirst state)
                                       infinity)))

(defrecord delay1 [processing-time]
  IModel
  (initial-total-state [model]
    (let [s {:queue (sorted-map)
             :delta 0}
          e 0]
      [s e]))
  (internal-update [model state]
    (-> state
        (update :queue dissoc (ffirst (:queue state)))
        (assoc :delta (ffirst (:queue state)))))
  (external-update [model state elapsed-time messages]
    (let [delta (+ (:delta state) elapsed-time)
          t     (+ delta processing-time)]
      (-> state
          (update-in [:queue t] into (:in messages))
          (assoc :delta delta))))
  (confluent-update [model state messages]
    (external-update model (internal-update model state) 0 messages))
  (partial-output [model state]
    {:out (second (first (:queue state)))})
  (time-advance [model state]
    (if (empty? (:queue state))
      infinity
      (- (ffirst (:queue state))
         (:delta state)))))

;; Like delay1, but messages are [processing-time value].
(defrecord delay2 []
  IModel
  (initial-total-state [model]
    (let [s {:queue (sorted-map)
             :delta 0}
          e 0]
      [s e]))
  (internal-update [model state]
    (-> state
        (update :queue dissoc (ffirst (:queue state)))
        (assoc :delta (ffirst (:queue state)))))
  (external-update [model state elapsed-time messages]
    (trace "external-update: %s" messages)
    (let [delta (+ (:delta state) elapsed-time)]
      (reduce (fn [state [processing-time value]]
                (let [t (+ delta processing-time)]
                  (update-in state [:queue t] conj value)))
              (assoc state :delta delta)
              (:in messages))))
  (confluent-update [model state messages]
    (external-update model (internal-update model state) 0 messages))
  (partial-output [model state]
    {:out (second (first (:queue state)))})
  (time-advance [model state]
    (if (empty? (:queue state))
      infinity
      (- (ffirst (:queue state))
         (:delta state)))))

(defrecord static-network-exec [network]
  IModel
  (initial-total-state [model]
    (let [s nil
          e 0]
      [s e]))
  (internal-update     [model state] state)
  (external-update     [model state elapsed messages] state)
  (confluent-update    [model state messages] state)
  (partial-output      [model state] nil)
  (time-advance        [model state] infinity)
  IExecutive
  (network-structure [model state]
    network))

(defrecord dynamic-network-exec [network-1 network-2 switch-time]
  IModel
  (initial-total-state [model]
    (let [s 1
          e 0]
      [s e]))
  (internal-update     [model state]
    (case state
      1 2
      2 1))
  (external-update     [model state elapsed messages] state)
  (confluent-update    [model state messages] state)
  (partial-output      [model state] nil)
  (time-advance        [model state] switch-time)
  IExecutive
  (network-structure [model state]
    (case state
      1 network-1
      2 network-2)))

(defrecord network [exec-name exec-model]
  INetwork
  (exec-name [model] exec-name)
  (exec-model [model] exec-model))

;;; Tests

(deftest simple-tests

  (is (= [[5  {:del-out ["test msg"]}]
          [15 {:del-out ["test msg 2"]}]]
         (let [gen     (lazy-seq-generator. [[0 {:out ["test msg"]}]
                                             [10 {:out ["test msg 2"]}]])
               del     (delay1. 5)
               network (-> empty-network
                           (add-model :gen gen)
                           (add-model :del del)
                           (connect [:gen :out :del :in identity])
                           (connect [:del :out network-name :del-out identity]))
               exec    (static-network-exec. network)
               net     (network. :exec exec)]
           (-> (model->sim :net net)
               (simulate 0)))))

  ;; Note that messages get dropped when they have been delivered to a delay,
  ;; but the delay is removed in a structure change.
  (is (= '[[1 {:gen-out ("msg-1")}]
           [2 {:gen-out ("msg-2"), :del-1-out ("msg-1")}]
           [3 {:gen-out ("msg-3"), :del-1-out ("msg-2")}]
           [4 {:gen-out ("msg-4"), :del-1-out ("msg-3")}]
           [5 {:gen-out ("msg-5"), :del-1-out ("msg-4")}]
           [6 {:gen-out ("msg-6")}]
           [7 {:gen-out ("msg-7")}]
           [8 {:gen-out ("msg-8"), :del-2-out ("msg-6")}]
           [9 {:gen-out ("msg-9"), :del-2-out ("msg-7")}]
           [10 {:gen-out ("msg-10"), :del-2-out ("msg-8")}]
           [11 {:gen-out ("msg-11")}]
           [12 {:gen-out ("msg-12"), :del-1-out ("msg-11")}]
           [13 {:gen-out ("msg-13"), :del-1-out ("msg-12")}]
           [14 {:gen-out ("msg-14"), :del-1-out ("msg-13")}]
           [15 {:gen-out ("msg-15"), :del-1-out ("msg-14")}]
           [16 {:gen-out ("msg-16")}]
           [17 {:gen-out ("msg-17")}]
           [18 {:gen-out ("msg-18"), :del-2-out ("msg-16")}]
           [19 {:gen-out ("msg-19"), :del-2-out ("msg-17")}]]
         (let [gen       (lazy-seq-generator. (for [i (range)] [1 {:out [(str "msg-" (inc i))]}]))
               del-1     (delay1. 1)
               del-2     (delay1. 2)
               network-1 (-> empty-network
                             (add-model :gen gen)
                             (add-model :del-1 del-1)
                             (connect [:gen :out :del-1 :in identity])
                             (connect [:gen :out network-name :gen-out identity])
                             (connect [:del-1 :out network-name :del-1-out identity]))
               network-2 (-> empty-network
                             (add-model :gen gen)
                             (add-model :del-2 del-2)
                             (connect [:gen :out :del-2 :in identity])
                             (connect [:gen :out network-name :gen-out identity])
                             (connect [:del-2 :out network-name :del-2-out identity]))
               exec      (dynamic-network-exec. network-1 network-2 5)
               net       (network. :exec exec)]
         (-> (model->sim :net net)
             (simulate 0 20))))))

;;------------------------------------------------------------------------------
;; Dynamic Structure Example
;;------------------------------------------------------------------------------

;;; id

(def next-id-atom (atom 0))

(defn next-id [] (swap! next-id-atom inc))

(defn reset-next-id! [] (reset! next-id-atom 0))

;;; server

(defn distribute-work [state]
  (trace "distribute-work")
  (loop [state state]
    (if (or (empty? (:queue state))
            (empty? (:workers state)))
      state
      (let [job    (peek (:queue state))
            _      (trace "job: %s" job)
            worker (peek (:workers state))]
        (trace "worker: %s" worker)
        (-> state
            (update :output assoc [:out worker] [[(:effort job ) (assoc job :worker worker :start-time *sim-time*)]])
            (update :queue pop)
            (update :workers pop)
            (assoc  :sigma 0))))))

(defn add-worker [state k model]
  (trace "add-worker")
  (update state :network (fn [network]
                           (-> network
                               (add-model k model)
                               (connect [k :out (:id state) [:out k] identity])
                               (connect [(:id state) [:out k] k :in identity])))))

(defn rem-worker [state k]
  (update state :network (fn [network]
                           (-> network
                               (rem-model k)
                               (disconnect [k :out (:id state) [:out k]])
                               (disconnect [(:id state) [:out k] k :in])))))

(defn maybe-grow [state]
  (trace "maybe-grow")
  (if (< (:capacity state) (count (:queue state)))
    (let [k (symbol (str "w" (next-id)))]
      (-> state
          (add-worker k (delay2.))
          (update :workers conj k)
          (update :capacity inc)
          distribute-work))
    state))

(defn maybe-shrink [state]
  (if (and (empty? (:queue state))
           (< 1 (:capacity state)))
    (-> state
        (rem-worker (first (:workers state)))
        (update :workers pop)
        (update :capacity dec)
        maybe-shrink)
    state))

(defn intake-jobs [state jobs]
  (update state :queue into jobs))

(defn export-artifacts [state vs]
  (-> state
      (update-in [:output :out] into vs)
      (assoc :sigma 0)))

(defrecord server-exec [id]
  ;;----------------------------------------------------------------------------
  IModel

  (initial-total-state [model]
    (let [s {:id       id
             :queue    queue
             :network  (-> empty-network
                           (connect [network-name :in id :in identity])
                           (connect [id :out network-name :out identity]))
             :workers  queue ;; A FIFO of available workers.
             :capacity 0
             :output   {}
             :sigma    infinity}
          e 0]
      [s e]))

  (internal-update     [model state]
    (trace "internal-update: %s" state)
    (assoc state :output {} :sigma infinity))

  (external-update     [model state elapsed messages]
    (trace "external-update: %s" messages)
    (reduce-kv (fn [state port vs]
                 (trace "ext: %s" vs)
                 (cond
                   (= port :in) (-> state
                                    (intake-jobs (map #(assoc % :arrival-time *sim-time*) vs))
                                    distribute-work
                                    maybe-grow)
                   :else        (-> state
                                    (export-artifacts (map #(assoc % :departure-time *sim-time*) vs))
                                    (update :workers conj (second port))
                                    distribute-work
                                    maybe-shrink)))
               state
               messages))

  (confluent-update    [model state messages]
    (trace "confluent-update")
    (external-update model (internal-update model state) 0 messages))

  (partial-output      [model state]
    (:output state))

  (time-advance        [model state]
    (:sigma state))

  ;;----------------------------------------------------------------------------
  IExecutive

  (network-structure [model state]
    (:network state)))

(defrecord server []
  INetwork
  (exec-name [model] :srv-exec)
  (exec-model [model] (server-exec. :srv-exec)))

;;; Tests

(defn report [log]
  (let [log          (->> log
                          (map second)
                          (mapcat :out)
                          (remove nil?))
        start-delays (map #(- (:start-time %) (:arrival-time %)) log)]
    (println "Total jobs: " (count log))
    (println "Total workers:" (count (distinct (map :worker log))))
    (println "Ave delay: " (float (/ (reduce + start-delays) (count start-delays))))
    (println "Max delay: " (apply max start-delays))))

(deftest dynamic-structure-test

  (time
   (binding [*trace* false]
     (let [gen (lazy-seq-generator. (for [i (range)]
                                      [(rand/rand-int 5) {:out [{:id     (str "job-" i)
                                                                 :effort (+ 1 (rand/rand-int 20))}]}]))
           server  (server.)
           network (-> empty-network
                       (add-model :gen gen)
                       (add-model :server server)
                       (connect [:gen :out :server :in identity])
                       (connect [:gen :out network-name :gen-out identity])
                       (connect [:server :out network-name :out identity]))
           exec    (static-network-exec. network)
           root    (network. :root-exec exec)]
       (reset-next-id!)
       (rand/with-random-seed 0
         (-> (model->sim :root root)
             (simulate 0 1000)
             report)))))

  )
