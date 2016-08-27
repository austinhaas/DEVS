(ns pettomato.devs.immediate-system
  (:require
   [pettomato.lib.number :refer [infinity]]
   [pettomato.devs.Simulator :refer [init int-update ext-update tl tn]]))

(defn immediate-system [sim start-time end-time ev*]
  (loop [sim (init sim start-time)
         ev* ev*
         acc (transient [])]
    (let [int-tn (tn sim)
          ext-tn (if (seq ev*) (ffirst ev*) infinity)]
      (cond
        (and (>= int-tn end-time)
             (>= ext-tn end-time)) (persistent! acc)
        (<= int-tn ext-tn)         (let [[sim' out] (int-update sim (tn sim))]
                                     (recur sim' ev* (reduce conj! acc (for [o out :when o] [(tn sim) o]))))
        :else                      (let [t               (ffirst ev*)
                                         [imminent ev*'] (split-with #(= (first %) t) ev*)]
                                     (recur (ext-update sim (map second imminent) t)
                                            ev*'
                                            acc))))))
