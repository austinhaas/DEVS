(ns des.dsdevs-simple
  (:require
   [clojure.core.async :as async :refer [timeout close! alts! go >!]]
   [pt-lib.number :refer [infinity]]
   [pt-lib.date :refer [now]]
   [pt-lib.coll :refer [group]]))

(defn model [initial-state int-update-fn ext-update-fn conf-update-fn output-fn time-advance-fn]
  {:type            ::atomic
   :initial-state   initial-state
   :int-update-fn   int-update-fn
   :ext-update-fn   ext-update-fn
   :conf-update-fn  (if (nil? conf-update-fn)
                      (fn [s e x] (ext-update-fn (int-update-fn s) 0 x))
                      conf-update-fn)
   :output-fn       output-fn
   :time-advance-fn time-advance-fn})

(defn network [initial-state int-update-fn ext-update-fn conf-update-fn output-fn time-advance-fn
               components connections]
  (-> (model initial-state int-update-fn ext-update-fn conf-update-fn output-fn time-advance-fn)
      (assoc :type        ::network
             :components  components
             :connections connections)))

(defn static-network [components connections]
  (network nil nil nil nil nil nil components connections))

(defn atomic?  [model] (= ::atomic  (:type model)))
(defn network? [model] (= ::network (:type model)))

;;------------------------------------------------------------------------------

;;; Priority queue.

(def empty-pq (sorted-map))

(defn pq-add [pq k v]
  ;; Don't add item if k is nil or infinity.
  (if (or (nil? k) (= k infinity))
    pq
    ;; Items are added to a set; no order is preserved.
    (update pq k (fnil conj #{}) v)))

(defn pq-rem [pq k v]
  (let [pq' (update pq k disj v)]
    (if (empty? (get pq' k))
      (dissoc pq' k)
      pq')))

(defn pq-update [pq k1 v k2]
  (if (= k1 k2)
    pq
    (-> pq (pq-rem k1 v) (pq-add k2 v))))

(defn pq-peek [pq] (second (first pq))) ;; Returns ALL items with highest priority.

(defn pq-pop [pq] (dissoc pq (ffirst pq))) ;; Removes ALL items with highest priority.

(defn pq-peek-key [pq] (ffirst pq))

;;; System

(defn init-model [m t]
  {:model m
   :state (:initial-state m)
   :tl    t
   :tn    (+ t ((:time-advance-fn m) (:initial-state m)))})

(defn init-network [m t]
  {:model      m
   :Q          empty-pq
   :tl         t
   :tn         ?
   :components })

(defn init [m]
  (case (:type m)
    :atomic (init-model m t)))

;; This would have to return a new state and output
(defn compute [s t]
  (if (= t (:tn s))
    (case (:type s)
      :atomic  (for [ev ((:output-fn (:model s)) (:state s))]
                 [k ev])
      :network (let [imminent (pq-peek (:Q s))
                     output   (for [[k v] (select-keys (:sims s) imminent)
                                    ev    (compute v)]
                                [k ev])]
                 ;; Route messages lateral and downward. Save in sim inbox.
                 ;; Return messages upward.
                 ))
    []))

(defn update-atomic-sim [s t]
  (let [{:keys [state model inbox tl tn]} s
        state' (if (= t tn)
                 (if (seq inbox)
                   ((:conf-update-fn model) inbox)
                   ((:int-update-fn model) state))
                 (let [e (- t tl)]
                   ((:ext-update-fn model) state e inbox)))]
    (assoc s
           :state state'
           :inbox (empty inbox)
           :tl    t
           :tn    (+ t ((:time-advance-fn model) state')))))

(defn update-network-sim [s t]
  ;; Find imminent sims + receiving sims.
  (let [sims  (:sims s)
        sims' (reduce-kv (fn [m k v] (assoc m k (update-sim v t )))
                         {}
                         sims)
        Q'    ()]
    (assoc s :sims sims')))

(defn update-sim [s t]
  (case (:type s)
    :atomic  (update-atomic-sim s t)
    :network (update-network-sim s t)))

(defn system [model start-time chan-in chan-out]
  (go
    (loop [sim   (init model start-time)
           tl    start-time
           wc-tl (now)]
      (let [tn     (:tn sim)
            wc-t   (now)
            tout   (let [wc-e (- wc-t wc-tl)
                         t    (min (+ tl wc-e) tn)]
                     (timeout (- tn t)))
            [v ch] (alts! [tout chan-in])]
        (if (and (= ch chan-in) (nil? v))
          (close! chan-out) ;; Done.
          (let [t    (condp = ch
                       chan-in (min (+ tl (- (now) wc-tl)) tn)
                       tout    tn)
                sim' (update-sim sim t [v])]
            (when (= t tn)
              (doseq [ev (compute sim)] (>! chan-out ev))) ;; TODO: port mappings
            (recur sim' t wc-t))))))
  true)
