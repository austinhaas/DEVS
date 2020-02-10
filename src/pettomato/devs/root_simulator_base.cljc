(ns pettomato.devs.root-simulator-base
  "Aka, part of a root-coordinator.

  In the literature, a root-coordinator is a closed system that runs
  the simulation. In this implementation, that behavior has been
  divided into two components:

  1. The root-simulator, defined in this namespace. This component
  wraps the supplied simulator with an API for stepping through the
  simulation, and scheduling events into the future.

  2. A system, which drives the root-simulator.

  This separation was motivated by the desire to abstract a base
  system (this namespace) from parts that may vary. For instance, for
  CLJS, a system could use intervals or requestAnimationFrame to drive
  the updates."
  (:require
   [pettomato.devs.Simulator :refer [init int-update ext-update con-update tl tn]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.log :as log]
   [pettomato.lib.queue :refer [queue]]))

(defn root-simulator [sim start-time]
  {:sim    (init sim start-time)
   :input  (pq/init)
   :output queue})

(defn step
  [pkg]
  (let [{:keys [sim input]} pkg]
    ;; int-tn - The time of the next internal update.
    ;; ext-tn - The time of the next external update.
    (let [int-tn (tn sim)
          ext-tn (or (pq/peek-key input) infinity)]
      (cond
        ;; No pending events.
        (and (= int-tn infinity)
             (= ext-tn infinity)) pkg
        ;; The sim internal update is next.
        (< int-tn ext-tn)         (let [[sim' out-msg*] (int-update sim int-tn)]
                                    (if (seq out-msg*)
                                      (-> pkg
                                          (assoc :sim sim')
                                          (update :output conj [int-tn out-msg*]))
                                      (-> pkg
                                          (assoc :sim sim'))))
        ;; An external message is next.
        (< ext-tn int-tn)         (let [t       (pq/peek-key input)
                                        in-msg* (pq/peek input)
                                        sim'    (ext-update sim in-msg* t)]
                                    (-> pkg
                                        (assoc :sim sim')
                                        (update :input pq/pop)))
        ;; The sim internal update is at the same time as the next external message.
        :else                     (let [t               (pq/peek-key input)
                                        in-msg*         (pq/peek input)
                                        [sim' out-msg*] (con-update sim in-msg* t)]
                                    (if (seq out-msg*)
                                      (-> pkg
                                          (assoc :sim sim')
                                          (update :input pq/pop)
                                          (update :output conj [int-tn out-msg*]))
                                      (-> pkg
                                          (assoc :sim sim')
                                          (update :input pq/pop))))))))

(defn schedule [pkg t msg]
  (when (< t (tl (:sim pkg)))
    (throw (ex-info "Cannot schedule event before last update time."
                    {:t   t
                     :msg msg
                     :tl  (tl (:sim pkg))})))
  (update pkg :input pq/insert t msg))

(defn schedule* [pkg tmsgs]
  (reduce (fn [pkg [t msg]] (schedule pkg t msg))
          pkg
          tmsgs))

(defn time-of-last-update [pkg]
  (tl (:sim pkg)))

(defn time-of-next-update [pkg]
  (let [{:keys [sim input]} pkg]
    (min (tn sim) (or (pq/peek-key input) infinity))))

(defn output [pkg]
  (vec (:output pkg)))

(defn clear-output [pkg]
  (update pkg :output empty))
