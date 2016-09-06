(ns pettomato.devs.immediate-system
  (:require
   [pettomato.lib.number :refer [infinity]]
   [pettomato.devs.Simulator :refer [init int-update ext-update tl tn]]))

(defn immediate-system [sim start-time end-time tmsg*]
  (loop [sim   (init sim start-time)
         tmsg* tmsg*
         acc   (transient [])]
    (let [int-tn (tn sim)
          ext-tn (if (seq tmsg*) (ffirst tmsg*) infinity)]
      (cond
        (and (>= int-tn end-time)
             (>= ext-tn end-time)) (persistent! acc)
        (<= int-tn ext-tn)         (let [[sim' out] (int-update sim (tn sim))]
                                     (recur sim' tmsg* (reduce conj! acc (for [o out :when o] [(tn sim) o]))))
        :else                      (let [t (ffirst tmsg*)
                                         [imminent tmsg*'] (split-with #(= (first %) t) tmsg*)]
                                     (recur (ext-update sim (map second imminent) t)
                                            tmsg*'
                                            acc))))))
