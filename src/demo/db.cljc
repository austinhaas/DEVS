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
                (contains? a k) [(assoc in-a k av) in-b]
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
                                     (conj acc {:pmap pmap
                                                :fmap fmap
                                                :old  old
                                                :new  new
                                                :oldf in-a
                                                :newf in-b})
                                     acc))
                                 acc))
                             []
                             (map vector old* new*))]
              (if (seq x*)
                (conj acc [[:sub-response id] x*])
                acc)))
          []
          sub*))

(defn db [primary-key & other-indices]
  (atomic-model
   {:db     (apply db/init primary-key other-indices)
    :subs   (subs-init)
    :output []
    :sigma  infinity}
   (fn int-update [s] (assoc s :output [] :sigma infinity))
   (fn ext-update [s e x]
     (reduce (fn [s msg]
               (match msg
                 [:insert m]      (let [db   (:db s)
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
                                          (assoc  :sigma  0))))
                 [:delete pmap]   (let [db    (:db s)
                                        old*  (db/query db pmap)
                                        e*    (map primary-key old*)
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
                                          (assoc  :sigma  0))))
                 [:modify m pmap] (let [db   (:db s)
                                        old* (db/query db pmap)
                                        e*   (map primary-key old*)
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
                                          (assoc  :sigma  0))))
                 [[:query id] pmap] (let [r (db/query (:db s) pmap)]
                                      (-> s
                                          (update :output conj [[:query-response id] [pmap r]])
                                          (assoc  :sigma  0)))
                 [[:sub   id] [pmap fmap]] (let [sub  [id pmap fmap]
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
                                                   (assoc  :sigma  0))))
                 [[:unsub id] [pmap fmap]] (let [sub [id pmap fmap]]
                                             (update s :subs subs-delete sub))))
             s
             x))
   nil
   :output
   :sigma))
