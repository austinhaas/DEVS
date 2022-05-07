(ns pettomato.devs.examples.models.server-queue
  "A dynamic structure example."
  (:require
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.coll :refer [queue]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.models.atomic-model :refer [time-advance]]
   [pettomato.devs.models.executive-model :refer [def-executive-model]]
   [pettomato.devs.models.network-model :refer [network-model]]))

;;; worker

(def ^:private worker m/buffer+)

;;; server

(defn- add-worker [state id [model elapsed]]
  (log/tracef "add-worker: %s" id)
  (update state :structure-changes conj
          [:add-model id [model elapsed]]
          [:connect [(:id state) [:out id] id :in]]
          [:connect [id :out (:id state) [:in id]]]))

(defn- rem-worker [state id]
  (log/tracef "rem-worker: %s" id)
  (update state :structure-changes conj
          [:rem-model id]
          [:disconnect [(:id state) [:out id] id :in]]
          [:disconnect [id :out (:id state) [:in id]]]))

(defn- maybe-grow [state]
  (log/trace "maybe-grow")
  (if (< (:capacity state) (count (:queue state)))
    (let [id (symbol (str "w" (:id-counter state)))]
      (-> state
          (add-worker id [(worker) h/zero])
          (update :workers conj id)
          (update :capacity inc)
          (update :id-counter inc)
          recur))
    state))

(defn- maybe-shrink [state]
  (log/tracef "maybe-shrink [jobs: %s idle: %s]" (count (:queue state)) (count (:workers state)))
  (if (and (empty? (:queue state))
           (< 1 (count (:workers state))))
    (-> state
        (rem-worker (peek (:workers state)))
        (update :workers pop)
        (update :capacity dec)
        recur)
    state))

(defn- distribute-work [state t]
  (log/tracef "distribute-work")
  (if (or (empty? (:queue state))
          (empty? (:workers state)))
    state
    (let [job    (peek (:queue state))
          worker (peek (:workers state))]
      (log/tracef "Assigning %s to %s" job worker)
      (-> state
          (update-in [:output [:out worker]] conj [(:effort job) (assoc job :worker worker :start-time t)])
          (update :queue pop)
          (update :workers pop)
          (assoc :sigma h/epsilon)))))

(defn- import-jobs [state jobs t]
  (let [jobs (map #(assoc % :arrival-time t) jobs)]
    (update state :queue into jobs)))

(defn- export-jobs [state worker jobs t]
  (let [jobs (map #(assoc % :departure-time t) jobs)]
    (-> state
        (update :workers conj worker)
        (update-in [:output :out] into jobs))))

(def-executive-model Server [id queue workers capacity output sigma structure-changes total-elapsed]
  (internal-update [state]
    (let [t (h/+ total-elapsed (time-advance state))]
      (-> (assoc state
                 :output {}
                 :structure-changes []
                 :sigma h/infinity
                 :total-elapsed t)
          (distribute-work t))))
  (external-update [state elapsed mail]
    (let [t (h/+ total-elapsed elapsed)]
      (reduce-kv (fn [state port vs]
                   (cond
                     ;; incoming jobs
                     (= port :in) (-> state
                                      (import-jobs vs t)
                                      maybe-grow)
                     ;; completed jobs
                     :else        (-> state
                                      (export-jobs (second port) vs t)
                                      maybe-shrink)))
                 (assoc state :sigma h/epsilon :total-elapsed t)
                 mail)))
  (output [state] output)
  (time-advance [state] sigma)
  (structure-changes [state] structure-changes))

(defn server
  "An model that processes jobs by delegating them to a dynamic pool of
  workers."
  [id]
  (network-model
   id
   [(map->Server {:id                id
                  :queue             queue ;; A FIFO of jobs.
                  :workers           queue ;; A FIFO of available workers.
                  :capacity          0
                  :output            {}
                  :sigma             h/infinity
                  :structure-changes []
                  :total-elapsed     h/zero
                  :id-counter        0})
    h/zero]
   {}
   [[:network :in id :in]
    [id :out :network :out]]))
