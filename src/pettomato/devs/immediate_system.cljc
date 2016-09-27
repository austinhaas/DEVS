(ns pettomato.devs.immediate-system
  (:require
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.Simulator :refer [init int-update ext-update con-update tl tn]]))

(defn immediate-step [sim start-time end-time tmsg*]
  (loop [sim   sim
         tmsg* tmsg*
         acc   (transient [])]
    (let [int-tn (tn sim)
          ext-tn (if (seq tmsg*) (ffirst tmsg*) infinity)]
      (cond
        (and (>= int-tn end-time)
             (>= ext-tn end-time)) [sim (persistent! acc)]
        (< int-tn ext-tn)          (let [[sim' out] (int-update sim (tn sim))
                                         acc'       (if (seq out)
                                                      (conj! acc [(tn sim) out])
                                                      acc)]
                                     (recur sim' tmsg* acc'))
        (< ext-tn int-tn)          (let [[[t msg*] & tmsg*'] tmsg*]
                                     (recur (ext-update sim msg* t)
                                            tmsg*'
                                            acc))
        :else                      (let [[[t msg*] & tmsg*'] tmsg*
                                         [sim' out] (con-update sim msg* t)
                                         acc'       (if (seq out)
                                                      (conj! acc [(tn sim) out])
                                                      acc)]
                                     (recur sim'
                                            tmsg*'
                                            acc'))))))

(defn immediate-system [sim start-time end-time tmsg*]
  (-> (immediate-step (init sim start-time) start-time end-time tmsg*)
      second))
