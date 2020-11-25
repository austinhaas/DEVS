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

#_
(route {:a {:x {:b {:y [identity]}
                :c {:y [identity]}}}
        :c {:y {:d {:x [identity]}}}
        :d {:x {:e {:y [identity]}}}}
       {:a {:x [100]}})

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

;;; Test models

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
   (constantly period)
   nil))

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
       infinity))
   nil))

(defn lazy-seq-generator-network-structure
  [s]
  (atomic-model
   (let [s s
         e 0]
     [s e])
   next
   nil
   nil
   (constantly nil)
   (fn time-advance [s]
     (if (seq s)
       (ffirst s)
       infinity))
   (comp second first)))

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
          (:delta state))))
   nil))

;; Like delay1, but messages are [processing-time value].
(defn delay2
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
          (:delta state))))
   nil))

;;; Tests

(deftest simple-tests

  (is (= [[5  {:del-out ["test msg"]}]
          [15 {:del-out ["test msg 2"]}]]
         (binding [*trace* true]
           (let [gen (lazy-seq-generator [[0 {:out ["test msg"]}]
                                          [10 {:out ["test msg 2"]}]])
                 del (delay1 5)
                 net (network-model {:gen gen
                                     :del del}
                                    [[:gen :out :del :in identity]
                                     [:del :out :network :del-out identity]])]
             (devs/run net 0)))))

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
         (binding [*trace* false]
           (let [gen   (lazy-seq-generator (for [i (range)] [1 {:out [(str "msg-" (inc i))]}]))
                 del-1 (delay1 1)
                 del-2 (delay1 2)
                 exec  (lazy-seq-generator-network-structure
                        (cycle [[5 [[:disconnect [:gen :out :del-1 :in identity]]
                                    [:disconnect [:del-1 :out :network :del-1-out identity]]
                                    [:rem-model :del-1 del-1]
                                    [:add-model :del-2 del-2]
                                    [:connect [:gen :out :del-2 :in identity]]
                                    [:connect [:del-2 :out :network :del-2-out identity]]]]
                                [5 [[:disconnect [:gen :out :del-2 :in identity]]
                                    [:disconnect [:del-2 :out :network :del-2-out identity]]
                                    [:rem-model :del-2 del-2]
                                    [:add-model :del-1 del-1]
                                    [:connect [:gen :out :del-1 :in identity]]
                                    [:connect [:del-1 :out :network :del-1-out identity]]]]]))
                 net   (network-model {:gen   gen
                                       :del-1 del-1
                                       :exec  exec}
                                      [[:gen :out :del-1 :in identity]
                                       [:gen :out :network :gen-out identity]
                                       [:del-1 :out :network :del-1-out identity]])]
             (devs/run net 0 20)))))

  )

(deftest structure-change-tests
  ;; TODO: Also remove without disconnecting.
  (testing "Remove an atomic model before it is imminent."
    (binding [*trace* false]
      (is (output= []
                   (let [gen  (lazy-seq-generator [[5 {:out ["msg 1"]}]
                                                   [10 {:out ["msg 2"]}]])
                         del  (delay1 2)
                         exec (lazy-seq-generator-network-structure
                               [[6 [[:disconnect [:gen :out :del :in identity]]
                                    [:disconnect [:del :out :network :out identity]]
                                    [:rem-model :del]]]])
                         net  (network-model {:gen  gen
                                              :del  del
                                              :exec exec}
                                             [[:gen :out :del :in identity]
                                              [:del :out :network :out identity]])]
                     (devs/run net 0))))))

  (testing "Remove an atomic model at the same time as it is imminent."
    (binding [*trace* false]
      (is (output= [[7 {:out ["msg 1"]}]]
                   (let [gen  (lazy-seq-generator [[5 {:out ["msg 1"]}]
                                                   [10 {:out ["msg 2"]}]])
                         del  (delay1 2)
                         exec (lazy-seq-generator-network-structure
                               [[7 [[:disconnect [:gen :out :del :in identity]]
                                    [:disconnect [:del :out :network :out identity]]
                                    [:rem-model :del]]]])
                         net  (network-model {:gen  gen
                                              :del  del
                                              :exec exec}
                                             [[:gen :out :del :in identity]
                                              [:del :out :network :out identity]])]
                     (devs/run net 0))))))

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
                         exec (lazy-seq-generator-network-structure
                               [[7 [[:disconnect [:gen :out :del :in identity]]
                                    [:disconnect [:del :out :network :out identity]]
                                    [:rem-model :del]]]])
                         net  (network-model {:gen  gen
                                              :del  del
                                              :exec exec}
                                             [[:gen :out :del :in identity]
                                              [:del :out :network :out identity]])]
                     (devs/run net 0))))))

  (testing "Remove a network model, after dynamic structure changes have been made."
    ;; An atom is used to count how many times the added model is updated, to
    ;; check that it was actually removed. We can't tell from the output alone,
    ;; because it could be sending messages, but the parent network's routes
    ;; have been removed, so they won't go anywhere.
    (binding [*trace* false]
      (let [counter (atom 0)
            net     (network-model
                     {:gen  (lazy-seq-generator [[5  {:out ["msg 1"]}]
                                                 [10 {:out ["msg 2"]}]])
                      :del  (network-model {:del  (delay1 2)
                                            :exec (lazy-seq-generator-network-structure
                                                   [[1 [[:add-model :gen (lazy-seq-generator
                                                                          (for [i (range)]
                                                                            (do (swap! counter inc)
                                                                                [1 {:out [(str "internal gen msg-" i)]}])))]
                                                        [:connect [:gen :out :network :out identity]]]]])}
                                           [[:network :in :del :in identity]
                                            [:del :out :network :out identity]])
                      :exec (lazy-seq-generator-network-structure
                             [[7 [[:disconnect [:gen :out :del :in identity]]
                                  [:disconnect [:del :out :network :out identity]]
                                  [:rem-model :del]]]])}
                                   [[:gen :out :del :in identity]
                                    [:del :out :network :out identity]])]
        (devs/run net 0 10)
        (is (= 7 @counter)))))

  ;; Test that structure changes happen from bottom up.
  ;; Remove the parent and the child.




  )

;;------------------------------------------------------------------------------
;; Dynamic Structure Example
;;------------------------------------------------------------------------------

;;; id

(def next-id-atom (atom 0))

(defn next-id [] (swap! next-id-atom inc))

(defn reset-next-id! [] (reset! next-id-atom 0))

;;; server

(defn add-worker [state k model]
  (trace "add-worker: %s" k)
  (update state :structure conj
          [:add-model k model]
          [:connect [(:id state) [:out k] k :in identity]]
          [:connect [k :out (:id state) [:in k] identity]]))

(defn rem-worker [state k]
  (trace "rem-worker: %s" k)
  (update state :structure conj
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
            :structure []
            :sigma     infinity}
         e 0]
      [s e])
    (fn internal-update     [state]
      (-> (assoc state :output {} :structure [] :sigma infinity)
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
    :sigma
    :structure))

;;; Tests

(defn report [log]
  (let [log          (->> log
                          (map second)
                          (mapcat :out)
                          (remove nil?)
                          (map #(assoc % :delay (- (:start-time %) (:arrival-time %)))))
        start-delays (map :delay log)]
    ;;(clojure.pprint/pprint log)
    (println "Total jobs: " (count log))
    (println "Total workers:" (count (distinct (map :worker log))))
    (println "Ave delay: " (float (/ (reduce + start-delays) (count start-delays))))
    (println "Max delay: " (apply max start-delays))))

(deftest dynamic-structure-test

  (time
   (binding [*trace*        false
             *print-length* 1000]
     (let [gen (lazy-seq-generator
                (take 10
                      (for [i (range)]
                        [(+ 1 (rand/rand-int 10)) {:out [{:id     (str "job-" i)
                                                          :effort (+ 1 (rand/rand-int 100))}]}])))
           srv (server :server)
           net (network-model
                {:gen    gen
                 :server srv}
                [[:gen :out :server :in identity]
                 [:gen :out :network :gen-out identity]
                 [:server :out :network :out identity]])]
       (reset-next-id!)
       (rand/with-random-seed 0
         (-> (devs/run net 0 1000)
             report)
         'done))))
  )

;; Test trie vs hash w/ vector key
(comment

  (rand/with-random-seed 0
   (let [ys (for [i (range 50000)]
              [(rand/rand-int 10) (rand/rand-int 2) (rand/rand-int 10) (rand/rand-int 2) (rand/rand-int 99999)])]
     (def xs (vec (rand/shuffle (concat ys ys))))))

  (def ys (mapv (partial mapv (comp keyword (partial str "key-")))
                xs))

  ;; Note that this doesn't prune dead branches.
  (time
   (do (reduce (fn [m [a b c d e]]
                 (if (get-in m [a b c d e])
                   (update-in m [a b c d] disj e)
                   (update-in m [a b c d] (fnil conj #{}) e)))
               {}
               ys)
       nil))

  (time
   (do (reduce (fn [m [a b c d e]]
                 (update-in m [a b c d e] (fnil inc 0)))
               {}
               ys)
       nil))


  (time
   (do (reduce (fn [s [a b c d e]]
                 (if (contains? s [a b c d e])
                   (disj s [a b c d e])
                   (conj s [a b c d e])))
               #{}
               ys)
       nil))

  (time
   (do (reduce (fn [s r]
                 (if (contains? s r)
                   (disj s r)
                   (conj s r)))
               #{}
               ys)
       nil))

  ;; Conclusion: Sets of vectors perform better. But, we'll need the trie for
  ;; lookups. However, we can do it as [name port] -> [name port].

  )
