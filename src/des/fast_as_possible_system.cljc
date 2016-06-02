(ns des.fast-as-possible-system
  (:require
   [pt-lib.number :refer [infinity]]
   [des.Simulator :refer [init int-update ext-update tl tn]]))

(defn fast-as-possible-system [sim start-time ev*]
  (loop [sim (init sim start-time)
         ev* ev*]
    (cond
      (and (empty? ev*)
           (= (tn sim) infinity))     'done
      (or (empty? ev*)
          (<= (tn sim) (ffirst ev*))) (let [[sim' out] (int-update sim (tn sim))]
                                        (doseq [o out] (println (format "t:%s> %s" (tn sim) o)))
                                        (recur sim' ev*))
      (< (ffirst ev*) (tn sim))       (let [t               (ffirst ev*)
                                            [imminent ev*'] (split-with #(= (first %) t) ev*)]
                                        (recur (ext-update sim (map second imminent) t)
                                               ev*')))))
