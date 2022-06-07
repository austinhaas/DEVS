(ns pettomato.devs.lib.mail
  "Functions for mail data structures.

  A mail data structure is a map from ids to a map from ports to a
  collection of values: id -> port -> values

  A local-mail data structure is just port -> values.

  A mail-log is a seq of [timestamp outbound-mail], where timestamps
  are strictly increasing and outbound-mail is a local-mail data
  structure."
  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.string :refer [pad-left]]))

(defn local-mail=
  "Compare two local-mail data structures for equality.

  A local-mail data structure is just port -> values."
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

(defn mail-log=
  "Compare mail-log data structures for equality."
  ([el] true)
  ([el1 el2]
   (or (and (empty? el1)
            (empty? el2))
       (let [[t1 mail1] (first el1)
             [t2 mail2] (first el2)]
         (and (boolean t1)
              (boolean t2)
              (h/= t1 t2)
              (local-mail= mail1 mail2)
              (recur (rest el1) (rest el2))))))
  ([el1 el2 & more]
   (if (mail-log= el1 el2)
     (if (next more)
       (recur el2 (first more) (next more))
       (mail-log= el2 (first more)))
     false)))

(defn pp-mail-log
  "Pretty-print a mail-log."
  [mail-log & {:keys [key-sort-fn
                      time-width]
               :or   {key-sort-fn (fn [a b] (compare (str a) (str b)))
                      time-width  6}}]
  (doseq [[t m] mail-log]
    (when (seq m)
      (print (str "[" (pad-left time-width \  (str t)) "] "))
      (let [[[k vs] & kvs] (sort-by first key-sort-fn m)]
        (println k "=>" (vec vs))
        (doseq [[k vs] kvs]
          (println (str (apply str (repeat (+ 3 time-width) \ )) (str k " => " (vec vs)))))
        #_(newline)))))

(defn format-mail-log
  "For developer convenience, convert seqs to vectors so that the output can be
  read in as valid data literals."
  [mail-log]
  (mapv (fn [[t mail]]
          [t (zipmap (keys mail) (map vec (vals mail)))])
        mail-log))
