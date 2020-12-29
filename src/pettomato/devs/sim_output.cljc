(ns pettomato.devs.sim-output
  (:require
   [pettomato.devs.lib.string :refer [pad-left]]
   [pettomato.devs.lib.mail :refer [mail=]]))

#_
(defn pp-output [xs & {:keys [key-sort-fn
                              time-width]
                       :or {key-sort-fn (fn [a b] (compare (str a) (str b)))
                            time-width 6}}]
  (doseq [[t m] xs]
    (println (pad-left time-width \  (str t)))
    (println (pad-left time-width \- "-")) ;; At least 1, even if time-width is less.
    (doseq [[k vs] (sort-by first key-sort-fn m)]
      (println k "=>" (vec vs)))
    (newline)))

(defn pp-output [xs & {:keys [key-sort-fn
                              time-width]
                       :or {key-sort-fn (fn [a b] (compare (str a) (str b)))
                            time-width 6}}]
  (doseq [[t m] xs]
    (print (str "[" (pad-left time-width \  (str t)) "] "))
    (let [[[k vs] & kvs] (sort-by first key-sort-fn m)]
      (println k "=>" (vec vs))
      (doseq [[k vs] kvs]
        (println (str (apply str (repeat (+ 3 time-width) \ )) (str k " => " (vec vs)))))
      (newline))))

(defn output=
  [expected actual]
  (or (and (empty? expected)
           (empty? actual))
      (let [[t mail] (first expected)
            [t' mail'] (first actual)]
        (and (= t t')
             (mail= mail mail')
             (output= (rest expected) (rest actual))))))

(defn format-output-messages
  "For developer convenience, convert seqs to vectors so that the output can be
  read in as valid data literals."
  [xs]
  (mapv (fn [[t mail]]
          [t (zipmap (keys mail) (map vec (vals mail)))])
        xs))
