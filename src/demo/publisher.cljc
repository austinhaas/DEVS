(ns demo.publisher
  (:require
   [pettomato.lib.db :as db]
   [pettomato.lib.number :refer [infinity]]
   [devs.models :refer [atomic-model]]))

;; Subscriptions are stored in a map, indexed by the attributes
;; contained in the pmap component of the sub.

(defn- subs-init [] {})

(defn- subs-insert [subs sub]
  (let [[id pmap] sub]
    (reduce (fn [m a] (update m a (fnil conj #{}) sub))
            subs
            (keys pmap))))

(defn- subs-delete [subs sub]
  (let [[id pmap] sub]
    (reduce (fn [m a] (update m a disj sub))
            subs
            (keys pmap))))

(defn- subs-query
  "Returns a seq containing all subs that watch any of the supplied
  attributes. In other words, the relevant subs for a given set of
  attributes."
  [subs attrs]
  (distinct (mapcat subs attrs)))

(defn- pub [s m]
  (let [sub* (subs-query (:subs s) (keys m))
        out* (reduce (fn [acc [id pmap]]
                       (if (db/pass? pmap m)
                         (conj acc [[:sub-response id] [pmap m]])
                         acc))
                     []
                     sub*)]
    (if (empty? out*)
      s
      (-> s
          (update :output into out*)
          (assoc  :sigma  0)))))

(defn- pub* [s x]
  (reduce pub s (map (fn [[port m]] m) x)))

(defn- sub [s id pmap]
  (update s :subs subs-insert [id pmap]))

(defn- sub* [s x]
  (reduce (fn [s [[port id] pmap]]
            (sub s id pmap))
          s
          x))

(defn- unsub [s id pmap]
  (update s :subs subs-delete [id pmap]))

(defn- unsub* [s x]
  (reduce (fn [s [[port id] pmap]]
            (unsub s id pmap))
          s
          x))

(defn- port-sym [ev] (if (vector? (first ev)) (ffirst ev) (first ev)))

(defn publisher []
  (atomic-model
   {:subs   (subs-init)
    :output []
    :sigma  infinity}
   (fn int-update [s] (assoc s :output [] :sigma infinity))
   (fn ext-update [s e x]
     (let [m (group-by port-sym x)]
       (-> s
           (pub*   (:pub   m))
           (sub*   (:sub   m))
           (unsub* (:unsub m)))))
   nil
   :output
   :sigma))
