(ns pettomato.devs.lib.mail
  "Functions for mail data structures.

  A mail data structure is a map from keys to a map from ports to a collection
  of values. key -> port -> values")

(defn mail=
  "Compare two mail data structures for equality."
  [m1 m2]
  (and (= (count m1)
          (count m2))
       (loop [kvs (seq m1)]
         (or (empty? kvs)
             (let [[k v] (first kvs)]
               (and (= (frequencies v)
                       (frequencies (get m2 k)))
                    (recur (rest kvs))))))))

(def merge-mail
  "Like clojure.core/merge, but specifically for mail data structures."
  (partial merge-with (partial merge-with into)))

(defn route-mail
  "Takes routes and outbound mail. Returns inbound mail.

  routes        - {sk {sp {rk {rp fs}}}}
  outbound mail - {sk {sp vs}}
  inbound mail  - {rk {rp vs}}"
  [routes mail]
  (reduce (fn [m [rk rp vs]]
            (update-in m [rk rp] into vs))
          {}
          (for [[sk sp->vs] mail
                [sp vs]     sp->vs
                [rk rp->fs] (get-in routes [sk sp])
                [rp fs]     rp->fs
                f           fs]
            [rk rp (map f vs)])))

(defn sort-mail
  "Groups inbound mail into three disjoint collections:
  [int-mail ext-mail net-msgs]."
  [mail]
  (let [int-mail (dissoc mail :network)
        ext-mail (get mail :network)
        net-msgs (get ext-mail :structure)
        ext-mail (dissoc ext-mail :structure)]
    [int-mail ext-mail net-msgs]))
