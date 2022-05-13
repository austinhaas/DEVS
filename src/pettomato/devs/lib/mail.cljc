(ns pettomato.devs.lib.mail
  "Functions for mail data structures.

  A mail data structure is a map from ids to a map from ports to a
  collection of values: id -> port -> values")

(defn local-mail=
  "Compare two \"local\" mail data structures for equality.

  A local mail data structure is just port -> values."
  [m1 m2]
  (and (= (count m1)
          (count m2))
       (loop [kvs (seq m1)]
         (or (empty? kvs)
             (let [[k v] (first kvs)]
               (and (= (frequencies v)
                       (frequencies (get m2 k)))
                    (recur (rest kvs))))))))

(def merge-local-mail
  "Like clojure.core/merge, but for local-mail data structures."
  (partial merge-with into))

(def merge-mail
  "Like clojure.core/merge, but for mail data structures."
  (partial merge-with merge-local-mail))

(defn route-mail
  "Takes routes and outbound mail. Returns inbound mail.

  routes        - sk -> sp -> rk -> rp -> txs
  outbound mail - sk -> sp -> vs
  inbound mail  - rk -> rp -> vs

  s   - sender
  r   - receiver
  k   - key (id)
  p   - port
  txs - transducers (to apply to the values on the route)
  vs  - values"
  [routes mail]
  (reduce (fn [m [rk rp vs]]
            (update-in m [rk rp] into vs))
          {}
          (for [[sk sp->vs]  mail
                [sp vs]      sp->vs
                [rk rp->txs] (get-in routes [sk sp])
                [rp txs]     rp->txs
                tx           txs]
            [rk rp (into [] tx vs)])))
