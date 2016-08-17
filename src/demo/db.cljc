(ns demo.db
  (:require
   [clojure.set :refer [subset? intersection union]]
   [pt-lib.db :as db]
   [pt-lib.number :refer [infinity]]
   [pt-lib.match :refer [match]]
   [devs.models :refer [atomic-model executive-model network-model add-component add-connection]]))

;; Subscriptions are stored in a map, indexed by the attributes
;; contained in the fmap component of the sub.

(defn- subs-init [] {})

(defn- subs-insert [subs sub]
  (let [[id pmap fmap] sub]
    (reduce (fn [m a] (update m a (fnil conj #{}) sub))
            subs
            (keys fmap))))

(defn- subs-delete [subs sub]
  (let [[id pmap fmap] sub]
    (reduce (fn [m a] (update m a disj sub))
            subs
            (keys fmap))))

(defn- subs-query
  "Returns a seq containing all subs that watch any of the supplied
  attributes. In other words, the relevant subs for a given set of
  attributes."
  [subs attrs]
  (distinct (mapcat subs attrs)))

(defn- diff-map
  "Returns [things-only-in-a things-only-in-b]. Not recursive."
  [a b]
  (reduce (fn [[in-a in-b] k]
            (let [av (get a k)
                  bv (get b k)]
              (cond
                (= av bv)       [in-a in-b]
                (contains? a k) (if (contains? b k)
                                  [(assoc in-a k av) (assoc in-b k bv)]
                                  [(assoc in-a k av) in-b])
                (contains? b k) [in-a (assoc in-b k bv)]
                :else           [(assoc in-a k av) (assoc in-b k bv)])))
          [{} {}]
          (distinct (concat (keys a) (keys b)))))

(defn- process-sub* [sub* old* new*]
  (reduce (fn [acc [id pmap fmap]]
            (let [x* (reduce (fn [acc [old new]]
                               (if (or (db/pass? pmap old)
                                       (db/pass? pmap new))
                                 (let [oldf (db/apply-fmap fmap old)
                                       newf (db/apply-fmap fmap new)
                                       [in-a in-b] (diff-map oldf newf)]
                                   (if (or (seq in-a) (seq in-b))
                                     (conj acc {:old  old
                                                :new  new
                                                :oldf in-a
                                                :newf in-b})
                                     acc))
                                 acc))
                             []
                             (map vector old* new*))]
              (if (seq x*)
                (conj acc [[:sub-response id] [[pmap fmap] x*]])
                acc)))
          []
          sub*))

(defn- db-insert [s m]
  (let [db   (:db s)
        db'  (db/insert db m)
        old* [{}] ;; No old for insert!
        new* [m]
        sub* (subs-query (:subs s) (keys m))
        out* (process-sub* sub* old* new*)]
    (if (empty? out*)
      (assoc s :db db')
      (-> s
          (assoc  :db     db')
          (update :output into out*)
          (assoc  :sigma  0)))))

(defn- db-insert* [s x]
  (reduce db-insert s (map (fn [[port m]] m) x)))

(defn- db-delete [s pmap]
  (let [db    (:db s)
        old*  (db/query db pmap)
        e*    (map (:key s) old*)
        db'   (db/delete-by-id* db e*)
        new*  (repeat {}) ;; No new for delete!
        attr* (distinct (mapcat keys old*))
        sub*  (subs-query (:subs s) attr*)
        out*  (process-sub* sub* old* new*)]
    (if (empty? out*)
      (assoc s :db db')
      (-> s
          (assoc  :db     db')
          (update :output into out*)
          (assoc  :sigma  0)))))

(defn- db-delete* [s x]
  (reduce db-delete s (map (fn [[port pmap]] pmap) x)))

(defn- db-modify [s m pmap]
  (let [db   (:db s)
        old* (db/query db pmap)
        e*   (map (:key s) old*)
        db'  (db/modify-by-id* db m e*)
        ;; The modified db is not queried w/ pmap, because
        ;; the modified values may no longer satisfy pmap.
        new* (db/query-by-id* db' e*)
        sub* (subs-query (:subs s) (keys m))
        out* (process-sub* sub* old* new*)]
    (if (empty? out*)
      (assoc s :db db')
      (-> s
          (assoc  :db     db')
          (update :output into out*)
          (assoc  :sigma  0)))))

(defn- db-modify* [s x]
  (reduce (fn [s [port [m pmap]]]
            (db-modify s m pmap))
          s
          x))

(defn- db-query [s id pmap]
  (let [r (db/query (:db s) pmap)]
    (-> s
        (update :output conj [[:query-response id] [pmap r]])
        (assoc  :sigma  0))))

(defn- db-query* [s x]
  (reduce (fn [s [[port id] pmap]]
            (db-query s id pmap))
          s
          x))

(defn- db-sub [s id pmap fmap]
  (let [sub  [id pmap fmap]
        subs (subs-insert (:subs s) sub)
        old* (repeat {})
        new* (db/query (:db s) pmap)
        sub* [sub]
        out* (process-sub* sub* old* new*)]
    (if (empty? out*)
      (assoc s :subs subs)
      (-> s
          (assoc  :subs   subs)
          (update :output into out*)
          (assoc  :sigma  0)))))

(defn- db-sub* [s x]
  (reduce (fn [s [[port id] [pmap fmap]]]
            (db-sub s id pmap fmap))
          s
          x))

(defn- db-unsub [s id pmap fmap]
  (let [sub [id pmap fmap]]
    (update s :subs subs-delete sub)))

(defn- db-unsub* [s x]
  (reduce (fn [s [[port id] [pmap fmap]]]
            (db-unsub s id pmap fmap))
          s
          x))

(defn- port-sym [ev] (if (vector? (first ev)) (ffirst ev) (first ev)))

(defn db [primary-key & other-indices]
  (atomic-model
   {:db     (apply db/init primary-key other-indices)
    :subs   (subs-init)
    :output []
    :sigma  infinity
    :key    primary-key}
   (fn int-update [s] (assoc s :output [] :sigma infinity))
   (fn ext-update [s e x]
     (let [m (group-by port-sym x)]
       (-> s
           (db-insert* (:insert m))
           (db-delete* (:delete m))
           (db-modify* (:modify m))
           (db-query*  (:query  m))
           (db-sub*    (:sub    m))
           (db-unsub*  (:unsub  m)))))
   nil
   :output
   :sigma))
