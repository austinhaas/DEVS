(ns des.fast-as-possible-system
  (:require
   [pt-lib.number :refer [infinity]]
   [des.Simulator :refer [init int-update ext-update tl tn]]))

(defn fast-as-possible-system [sim start-time end-time ev*]
  (loop [sim (init sim start-time)
         ev* ev*]
    (let [int-tn (tn sim)
          ext-tn (if (seq ev*) (ffirst ev*) infinity)]
      (cond
        (and (>= int-tn end-time)
             (>= ext-tn end-time)) 'done
        (<= int-tn ext-tn)         (let [[sim' out] (int-update sim (tn sim))]
                                     (doseq [o out] (println (format "[%s] %s" (tn sim) o)))
                                     (recur sim' ev*))
        :else                      (let [t               (ffirst ev*)
                                         [imminent ev*'] (split-with #(= (first %) t) ev*)]
                                     (recur (ext-update sim (map second imminent) t)
                                            ev*'))))))
