(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.lib.random :as rand]
   [pettomato.devs :as devs :refer [atomic-model network-model
                                    trace
                                    *trace*
                                    *sim-time*]]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.queue :refer [queue]]))

;;------------------------------------------------------------------------------
;; Test helpers

(defn mail= [m1 m2]
  (and (= (count m1)
          (count m2))
       (loop [kvs (seq m1)]
         (or (empty? kvs)
             (let [[k v] (first kvs)]
               (and (= (frequencies v)
                       (frequencies (get m2 k)))
                    (recur (rest kvs))))))))

(defn output=
  [expected actual]
  (or (and (empty? expected)
           (empty? actual))
      (let [[t mail] (first expected)
            [t' mail'] (first actual)]
        (and (= t t')
             (mail= mail mail')
             (output= (rest expected) (rest actual))))))

;;------------------------------------------------------------------------------
;; Simple function tests

(deftest prune-test

  (is (= {:x #{1}}
         (devs/prune {:x #{1}} [])))

  (is (= {:x #{1}}
         (devs/prune {:x #{1} :y {:z []}} [:y :z]))))

;;------------------------------------------------------------------------------
;; Test models

(defn generator
  "A model that periodically emits value on a port labeled :out."
  [period value]
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

(defn delay1
  [processing-time]
  (atomic-model
   (let [s {:queue (sorted-map)
            :delta 0}
         e 0]
     [s e])
   (fn internal-update  [state]
     (-> state
         (update :queue dissoc (ffirst (:queue state)))
         (assoc :delta (ffirst (:queue state)))))
   (fn external-update  [state elapsed-time messages]
     (let [delta (+ (:delta state) elapsed-time)
           t     (+ delta processing-time)]
       (-> state
           (update-in [:queue t] into (:in messages))
           (assoc :delta delta))))
   nil
   (fn output           [state]
     {:out (second (first (:queue state)))})
   (fn time-advance     [state]
     (if (empty? (:queue state))
       infinity
       (- (ffirst (:queue state))
          (:delta state))))))

;;------------------------------------------------------------------------------
;; Tests

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (is (= [[2 {:out ["x"]}]
            [4 {:out ["x"]}]
            [6 {:out ["x"]}]
            [8 {:out ["x"]}]]
           (binding [*trace* false]
             (-> (generator 2 "x")
                 devs/atomic-simulator
                 (devs/run :end 10))))))

  (testing "Specifying a non-zero start time."
    (is (= [[7 {:out ["x"]}]
            [9 {:out ["x"]}]]
           (binding [*trace* false]
             (-> (generator 2 "x")
                 devs/atomic-simulator
                 (devs/run :start 5 :end 10))))))

  (testing "A simple network."
    (is (= [[5  {:out ["x"]}]
            [15 {:out ["y"]}]]
           (binding [*trace* false]
             (let [gen (lazy-seq-generator [[0  {:out ["x"]}]
                                            [10 {:out ["y"]}]])
                   del (delay1 5)
                   net (network-model {:gen gen
                                       :del del}
                                      [[:gen :out :del :in identity]
                                       [:del :out :network :out identity]])]
               (-> (devs/network-simulator net)
                   devs/run)))))))

(deftest deep-delay-network

  (testing "A deeply nested delay network"
    (let [delay-network-constructor
          (fn [del] (network-model {:del del}
                                   [[:network :in :del :in identity]
                                    [:del :out :network :out identity]]))]
      (binding [*trace* false]
        (is (= [[7 {:out ["x"]}]
                [9 {:out ["x"]}]]
               (let [gen (generator 2 "x")
                     del (-> (delay1 5)
                             delay-network-constructor
                             delay-network-constructor
                             delay-network-constructor
                             delay-network-constructor)
                     net (network-model {:gen gen
                                         :del del}
                                        [[:gen :out :del :in identity]
                                         [:del :out :network :out identity]])]
                 (-> (devs/network-simulator net)
                     (devs/run :end 10)))))))))

(defn dynamic-delay-network
  "Constructs a network that contains gen and adds del at start and removes it at
  end. gen is connected to del and del is connected to the network."
  [gen del start end]
  (network-model {:gen  gen
                  :exec (lazy-seq-generator
                         [[start {:out [[:add-model :del del]
                                        [:connect [:gen :out :del :in identity]]
                                        [:connect [:del :out :network :out identity]]]}]
                          [end {:out [[:disconnect [:gen :out :del :in identity]]
                                      [:disconnect [:del :out :network :out identity]]
                                      [:rem-model :del]]}]])}
                 [[:exec :out :network :structure identity]]))

(deftest structure-change-tests

  (testing "Remove an atomic model before it is imminent."
    (binding [*trace* false]
      (is (output= []
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    0 6)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 10)))))))

  (testing "Remove an atomic model when it is imminent."
    (binding [*trace* false]
      (is (output= [[7 {:out ["x"]}]]
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    0 7)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 10)))))))

  (testing "Remove an atomic model after it is imminent."
    (binding [*trace* false]
      (is (output= [[7 {:out ["x"]}]]
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    0 8)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 10)))))))

  (testing "Adding an atomic model after it would have received input."
    (binding [*trace* false]
      (is (output= [[12 {:out ["x"]}]
                    [17 {:out ["x"]}]]
                   (let [net (dynamic-delay-network (generator 5 "x")
                                                    (delay1 2)
                                                    5 15)]
                     (-> (devs/network-simulator net)
                         (devs/run :start 0 :end 20)))))))

  ;; Test that structure changes happen from bottom up.
  ;; Remove the parent and the child.

  )

(deftest ad-hoc-tests

  (testing "Remove a network model"
    (binding [*trace* false]
      (is (output= [[7 {:out ["msg 1" "Good"]}]]
                   (let [gen  (lazy-seq-generator [[5 {:out ["msg 1"]}]
                                                   [10 {:out ["msg 2"]}]])
                         del  (network-model {:del  (delay1 2)
                                              :gen2 (lazy-seq-generator [[7 {:out ["Good"]}]
                                                                         [8 {:out ["Bad"]}]])}
                                             [[:network :in :del :in identity]
                                              [:del :out :network :out identity]
                                              [:gen2 :out :network :out identity]])
                         exec (lazy-seq-generator
                               [[7 {:out [[:disconnect [:gen :out :del :in identity]]
                                          [:disconnect [:del :out :network :out identity]]
                                          [:rem-model :del]]}]])
                         net  (network-model {:gen  gen
                                              :del  del
                                              :exec exec}
                                             [[:gen :out :del :in identity]
                                              [:del :out :network :out identity]
                                              [:exec :out :network :structure identity]])]
                     (-> (devs/network-simulator net)
                         devs/run))))))

  (testing "ad-hoc network structure change test"
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
           (binding [*trace*        false
                     *print-length* 100]
             (let [gen   (lazy-seq-generator (for [i (range)] [1 {:out [(str "msg-" (inc i))]}]))
                   del-1 (delay1 1)
                   del-2 (delay1 2)
                   exec  (lazy-seq-generator
                          (cycle [[5 {:out [[:disconnect [:gen :out :del-1 :in identity]]
                                            [:disconnect [:del-1 :out :network :del-1-out identity]]
                                            [:rem-model :del-1 del-1]
                                            [:add-model :del-2 del-2]
                                            [:connect [:gen :out :del-2 :in identity]]
                                            [:connect [:del-2 :out :network :del-2-out identity]]]}]
                                  [5 {:out [[:disconnect [:gen :out :del-2 :in identity]]
                                            [:disconnect [:del-2 :out :network :del-2-out identity]]
                                            [:rem-model :del-2 del-2]
                                            [:add-model :del-1 del-1]
                                            [:connect [:gen :out :del-1 :in identity]]
                                            [:connect [:del-1 :out :network :del-1-out identity]]]}]]))
                   net   (network-model {:gen   gen
                                         :del-1 del-1
                                         :exec  exec}
                                        [[:gen :out :del-1 :in identity]
                                         [:gen :out :network :gen-out identity]
                                         [:del-1 :out :network :del-1-out identity]
                                         [:exec :out :network :structure identity]])]
               (-> (devs/network-simulator net)
                   (devs/run :start 0 :end 20))))))))

;;------------------------------------------------------------------------------
;; A complex example
;;------------------------------------------------------------------------------

;;; id

(def next-id-atom (atom 0))

(defn next-id [] (swap! next-id-atom inc))

(defn reset-next-id! [] (reset! next-id-atom 0))

;;; worker

(defn delay2
  "Like delay1, but messages are [processing-time value]."
  []
  (atomic-model
   (let [s {:queue (sorted-map)
            :delta 0}
         e 0]
     [s e])
   (fn internal-update  [state]
     (-> state
         (update :queue dissoc (ffirst (:queue state)))
         (assoc :delta (ffirst (:queue state)))))
   (fn external-update  [state elapsed-time messages]
     (trace "external-update: %s" messages)
     (let [delta (+ (:delta state) elapsed-time)]
       (reduce (fn [state [processing-time value]]
                 (let [t (+ delta processing-time)]
                   (update-in state [:queue t] conj value)))
               (assoc state :delta delta)
               (:in messages))))
   nil
   (fn output           [state]
     {:out (second (first (:queue state)))})
   (fn time-advance     [state]
     (if (empty? (:queue state))
       infinity
       (- (ffirst (:queue state))
          (:delta state))))))

;;; server

(defn add-worker [state k model]
  (trace "add-worker: %s" k)
  (update-in state [:output :structure] conj
             [:add-model k model]
             [:connect [(:id state) [:out k] k :in identity]]
             [:connect [k :out (:id state) [:in k] identity]]))

(defn rem-worker [state k]
  (trace "rem-worker: %s" k)
  (update-in state [:output :structure] conj
          [:rem-model k]
          [:disconnect [(:id state) [:out k] k :in identity]]
          [:disconnect [k :out (:id state) [:in k] identity]]))

(defn distribute-work [state]
  (trace "distribute-work")
  (if (or (empty? (:queue state))
          (empty? (:workers state)))
    state
    (let [job    (peek (:queue state))
          worker (peek (:workers state))]
      (trace "Assigning %s to %s" job worker)
      (-> state
          (update :output assoc [:out worker] [[(:effort job) (assoc job :worker worker :start-time *sim-time*)]])
          (update :queue pop)
          (update :workers pop)
          (assoc  :sigma 0)))))

(defn maybe-grow [state]
  (trace "maybe-grow")
  (if (< (:capacity state) (count (:queue state)))
    (let [k (symbol (str "w" (next-id)))]
      (-> state
          (add-worker k (delay2))
          (update :workers conj k)
          (update :capacity inc)
          recur))
    state))

(defn maybe-shrink [state]
  (trace "maybe-shrink [jobs: %s idle: %s]" (count (:queue state)) (count (:workers state)))
  (if (and (empty? (:queue state))
           (< 1 (count (:workers state))))
    (-> state
        (rem-worker (peek (:workers state)))
        (update :workers pop)
        (update :capacity dec)
        recur)
    state))

(defn intake-jobs [state jobs]
  (let [jobs' (map #(assoc % :arrival-time *sim-time*) jobs)]
    (update state :queue into jobs')))

(defn finish-jobs [state worker jobs]
  (let [jobs' (map #(assoc % :departure-time *sim-time*) jobs)]
    (-> state
        (update :workers conj worker)
        (update-in [:output :out] into jobs'))))

(defn server [id]
  (atomic-model
   (let [s {:id        id
            :queue     queue
            :workers   queue ;; A FIFO of available workers.
            :capacity  0
            :output    {}
            :sigma     infinity}
         e 0]
      [s e])
    (fn internal-update     [state]
      (-> (assoc state :output {} :sigma infinity)
          distribute-work))
    (fn external-update     [state elapsed messages]
      (reduce-kv (fn [state port vs]
                   (cond
                     ;; incoming jobs
                     (= port :in) (-> state
                                      (intake-jobs vs)
                                      maybe-grow)
                     ;; completed jobs
                     :else        (-> state
                                      (finish-jobs (second port) vs)
                                      maybe-shrink)))
                 (assoc state :sigma 0)
                 messages))
    nil
    :output
    :sigma))

;;; Tests

(defn report [log]
  (let [log          (->> log
                          (map second)
                          (mapcat :out)
                          (remove nil?)
                          (map #(assoc % :delay (- (:start-time %) (:arrival-time %)))))
        start-delays (map :delay log)]
    {:total-jobs    (count log)
     :total-workers (count (distinct (map :worker log)))
     :ave-delay     (/ (reduce + start-delays) (count start-delays))
     :max-delay     (apply max start-delays)}))

(deftest complex-example-test

  (is (= {:total-jobs    100
          :total-workers 10
          :ave-delay     (/ 2489 100)
          :max-delay     48}
         (binding [*trace*        false
                   *print-length* 1000]
           (let [gen (lazy-seq-generator
                      (take 100
                            (for [i (range)]
                              [(+ 1 (rand/rand-int 10)) {:out [{:id     (str "job-" i)
                                                                :effort (+ 1 (rand/rand-int 100))}]}])))
                 srv (server :server)
                 net (network-model
                      {:gen    gen
                       :server srv}
                      [[:gen :out :server :in identity]
                       [:gen :out :network :gen-out identity]
                       [:server :out :network :out identity]
                       [:server :structure :network :structure identity]])]
             (reset-next-id!)
             (rand/with-random-seed 0
               (-> (devs/run (devs/network-simulator net) :start 0 :end 1000)
                   report)))))))
