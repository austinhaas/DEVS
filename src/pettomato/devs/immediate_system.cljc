(ns pettomato.devs.immediate-system
  (:require
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.Simulator :refer [init int-update ext-update con-update tl tn]]))

(defn immediate-step
  "Advances the simulation by one step.

   sim - A DEVS simulation.

   start-time - The time at the start of the step.

   end-time - The time at the end of the step.

   tmsg-in - (Optional.) A collection of timestamped messages
   (i.e., [timestamp message]), sorted by timestamp, that are passed
   to the simulation for this time step. Any message with a timestamp
   greater than or equal to end-time will be ignored. I'm not sure what
   happens if an input message has a timestamp < start-time.

   Returns a collection of timestamped messages, sorted by timestamp,
   that the simulation outputs during the step."
  ([sim start-time end-time]
   (immediate-step sim start-time end-time []))
  ([sim start-time end-time tmsg-in]
   (loop [sim      sim
          tmsg-in  tmsg-in
          tmsg-out (transient [])]
     ;; int-tn - The time of the next internal update.
     ;; ext-tn - The time of the next external update.
     (let [int-tn (tn sim)
           ext-tn (if (seq tmsg-in) (ffirst tmsg-in) infinity)]
       (cond

         (and (<= end-time int-tn)
              (<= end-time ext-tn)) [sim (persistent! tmsg-out)]

         (< int-tn ext-tn)          (let [[sim' msg] (int-update sim int-tn)
                                          tmsg-out'  (if (seq msg)
                                                       (conj! tmsg-out [int-tn msg])
                                                       tmsg-out)]
                                      (recur sim' tmsg-in tmsg-out'))

         (< ext-tn int-tn)          (let [[[t msg*] & tmsg-in'] tmsg-in]
                                      (recur (ext-update sim msg* t)
                                             tmsg-in'
                                             tmsg-out))

         :else                      (let [[[t msg*] & tmsg-in'] tmsg-in
                                          [sim' out] (con-update sim msg* t)
                                          tmsg-out'  (if (seq out)
                                                       (conj! tmsg-out [int-tn out])
                                                       tmsg-out)]
                                      (recur sim'
                                             tmsg-in'
                                             tmsg-out')))))))

(defn immediate-system
  ([sim start-time end-time] (immediate-system sim start-time end-time []))
  ([sim start-time end-time tmsg-in]
   (-> (immediate-step (init sim start-time) start-time end-time tmsg-in)
       second)))
