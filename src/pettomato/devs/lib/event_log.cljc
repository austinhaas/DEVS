(ns pettomato.devs.lib.event-log
  "An event log is a seq of [timestamp mail].

  mail - A map from ports to values."
  (:require
   [pettomato.devs.lib.string :refer [pad-left]]
   [pettomato.devs.lib.mail :refer [mail=]]))

(defn event-log=
  "Compare two event-log data structures for equality."
  [el1 el2]
  (or (and (empty? el1)
           (empty? el2))
      (let [[t1 mail1] (first el1)
            [t2 mail2] (first el2)]
        (and (= t1 t2)
             (mail= mail1 mail2)
             (event-log= (rest el1) (rest el2))))))

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

(defn pp-event-log [event-log & {:keys [key-sort-fn
                                        time-width]
                                 :or   {key-sort-fn (fn [a b] (compare (str a) (str b)))
                                        time-width  6}}]
  (doseq [[t m] event-log]
    (print (str "[" (pad-left time-width \  (str t)) "] "))
    (let [[[k vs] & kvs] (sort-by first key-sort-fn m)]
      (println k "=>" (vec vs))
      (doseq [[k vs] kvs]
        (println (str (apply str (repeat (+ 3 time-width) \ )) (str k " => " (vec vs)))))
      (newline))))

(defn format-event-log
  "For developer convenience, convert seqs to vectors so that the output can be
  read in as valid data literals."
  [event-log]
  (mapv (fn [[t mail]]
          [t (zipmap (keys mail) (map vec (vals mail)))])
        event-log))
