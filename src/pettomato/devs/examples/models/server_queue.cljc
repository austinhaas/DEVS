(ns pettomato.devs.examples.models.server-queue
  (:require
   [pettomato.devs :as devs :refer [infinity atomic-model trace *sim-time*]]
   [pettomato.devs.examples.models :refer [delay2]]
   [pettomato.lib.queue :refer [queue]]))

;;; id

(def ^:private next-id-atom (atom 0))

(defn- next-id [] (swap! next-id-atom inc))

(defn reset-next-id! [] (reset! next-id-atom 0))

;;; worker

(def ^:private worker delay2)

;;; server

(defn- add-worker [state k model]
  (trace "add-worker: %s" k)
  (update-in state [:output :structure] conj
             [:add-model k model]
             [:connect [(:id state) [:out k] k :in identity]]
             [:connect [k :out (:id state) [:in k] identity]]))

(defn- rem-worker [state k]
  (trace "rem-worker: %s" k)
  (update-in state [:output :structure] conj
          [:rem-model k]
          [:disconnect [(:id state) [:out k] k :in identity]]
          [:disconnect [k :out (:id state) [:in k] identity]]))

(defn- distribute-work [state]
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

(defn- maybe-grow [state]
  (trace "maybe-grow")
  (if (< (:capacity state) (count (:queue state)))
    (let [k (symbol (str "w" (next-id)))]
      (-> state
          (add-worker k (worker))
          (update :workers conj k)
          (update :capacity inc)
          recur))
    state))

(defn- maybe-shrink [state]
  (trace "maybe-shrink [jobs: %s idle: %s]" (count (:queue state)) (count (:workers state)))
  (if (and (empty? (:queue state))
           (< 1 (count (:workers state))))
    (-> state
        (rem-worker (peek (:workers state)))
        (update :workers pop)
        (update :capacity dec)
        recur)
    state))

(defn- intake-jobs [state jobs]
  (let [jobs' (map #(assoc % :arrival-time *sim-time*) jobs)]
    (update state :queue into jobs')))

(defn- finish-jobs [state worker jobs]
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
