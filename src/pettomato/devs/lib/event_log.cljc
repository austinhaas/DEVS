(ns pettomato.devs.lib.event-log
  "An event log is a seq of [timestamp outbound-mail], where timestamps are
  nondecreasing and outbound-mail is a \"local\" mail data structure (i.e., just
  port -> vals)."

  ;; TODO: Replace nondecreasing with increasing, given the NIA?

  (:require
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.string :refer [pad-left]]
   [pettomato.devs.lib.mail :refer [local-mail=]]))

(defn event-log=
  "Compare event-log data structures for equality."
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
   (if (event-log= el1 el2)
     (if (next more)
       (recur el2 (first more) (next more))
       (event-log= el2 (first more)))
     false)))

#_
(defn pp-event-log [event-log & {:keys [key-sort-fn
                                        time-width]
                                 :or   {key-sort-fn (fn [a b] (compare (str a) (str b)))
                                        time-width  6}}]
  (doseq [[t m] event-log]
    (println (pad-left time-width \  (str t)))
    (println (pad-left time-width \- "-")) ;; At least 1, even if time-width is less.
    (doseq [[k vs] (sort-by first key-sort-fn m)]
      (println k "=>" (vec vs)))
    (newline)))

(defn pp-event-log
  "Pretty-print an event log."
  [event-log & {:keys [key-sort-fn
                       time-width]
                :or   {key-sort-fn (fn [a b] (compare (str a) (str b)))
                       time-width  6}}]
  (doseq [[t m] event-log]
    (when (seq m)
      (print (str "[" (pad-left time-width \  (str t)) "] "))
      (let [[[k vs] & kvs] (sort-by first key-sort-fn m)]
        (println k "=>" (vec vs))
        (doseq [[k vs] kvs]
          (println (str (apply str (repeat (+ 3 time-width) \ )) (str k " => " (vec vs)))))
        #_(newline)))))

(defn format-event-log
  "For developer convenience, convert seqs to vectors so that the output can be
  read in as valid data literals."
  [event-log]
  (mapv (fn [[t mail]]
          [t (zipmap (keys mail) (map vec (vals mail)))])
        event-log))
