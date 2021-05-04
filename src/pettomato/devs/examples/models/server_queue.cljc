(ns pettomato.devs.examples.models.server-queue
  "A dynamic structure example."
  (:require
   [pettomato.devs.examples.models :refer [variable-delay]]
   [pettomato.devs.lib.coll :refer [queue]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.vars :refer [*sim-time*]]))

;;; id

(def ^:private next-id-atom (atom 0))

(defn- next-id [] (swap! next-id-atom inc))

(defn reset-next-id! [] (reset! next-id-atom 0))

;;; worker

(def ^:private worker variable-delay)

;;; server

(defn- add-worker [state k model]
  (log/tracef "add-worker: %s" k)
  (update-in state [:output :structure] conj
             [:add-model k model]
             [:connect [(:id state) [:out k] k :in identity]]
             [:connect [k :out (:id state) [:in k] identity]]))

(defn- rem-worker [state k]
  (log/tracef "rem-worker: %s" k)
  (update-in state [:output :structure] conj
             [:rem-model k]
             [:disconnect [(:id state) [:out k] k :in identity]]
             [:disconnect [k :out (:id state) [:in k] identity]]))

(defn- maybe-grow [state]
  (log/trace "maybe-grow")
  (if (< (:capacity state) (count (:queue state)))
    (let [k (symbol (str "w" (next-id)))]
      (-> state
          (add-worker k (worker))
          (update :workers conj k)
          (update :capacity inc)
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

(defn- distribute-work [state]
  (log/tracef "distribute-work")
  (if (or (empty? (:queue state))
          (empty? (:workers state)))
    state
    (let [job    (peek (:queue state))
          worker (peek (:workers state))]
      (log/tracef "Assigning %s to %s" job worker)
      (-> state
          (update :output assoc [:out worker] [[(:effort job) (assoc job :worker worker :start-time *sim-time*)]])
          (update :queue pop)
          (update :workers pop)
          (assoc  :sigma 0)))))

(defn- import-jobs [state jobs]
  (->> jobs
       (map #(assoc % :arrival-time *sim-time*))
       (update state :queue into)))

(defn- export-jobs [state worker jobs]
  (let [state (-> state
                  (update :workers conj worker))]
    (->> jobs
         (map #(assoc % :departure-time *sim-time*))
         (update-in state [:output :out] into))))

(defn server
  "An atomic model that processes jobs by delegating them to a dynamic pool of
  workers. Assumes that the containing network supports network structure
  messages."
  [id]
  (atomic-model
   :initial-state   {:id       id
                     :queue    queue ;; A FIFO of jobs.
                     :workers  queue ;; A FIFO of available workers.
                     :capacity 0
                     :output   {}
                     :sigma    infinity}
   :internal-update (fn [state]
                      (-> (assoc state :output {} :sigma infinity)
                          distribute-work))
   :external-update (fn [state elapsed messages]
                      (reduce-kv (fn [state port vs]
                                   (cond
                                     ;; incoming jobs
                                     (= port :in) (-> state
                                                      (import-jobs vs)
                                                      maybe-grow)
                                     ;; completed jobs
                                     :else        (-> state
                                                      (export-jobs (second port) vs)
                                                      maybe-shrink)))
                                 (assoc state :sigma 0)
                                 messages))
   :output          :output
   :time-advance    :sigma))
