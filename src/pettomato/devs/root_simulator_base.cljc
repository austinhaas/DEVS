(ns pettomato.devs.root-simulator-base
  "The core of a root-simulator, but without an \"update driver\".

  This core provides a priority-queue for scheduling future events, and a step
  function to update the simulation based on the next event, which may be
  internal, external, or both simultaneously.

  A complete root-simulator implementation depends on the platform and
  requirements. Some examples:

  - An \"as-fast-as-possible\" simulator may run through an entire simulation in a single loop.
  - A Real-Time simulator might synchronize the updates with a real-time clock.
  - In the browser, a Real-Time simulator might use setInterval or updateAnimationFrame to drive the updates.
  - On the JVM, a Real-Time simulator might use threads.

  All cases need a way to schedule future events and all cases use the same
  logic for determining the next event and processing it."
  (:require
   [pettomato.devs.Simulator :refer [init int-update ext-update con-update tl tn]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.lib.log :as log]))

(defn root-simulator [sim start-time]
  {:sim   (init sim start-time)
   :input (pq/init)})

(defn time-of-last-update [pkg]
  (tl (:sim pkg)))

(defn- time-of-next-internal-update [pkg]
  (tn (:sim pkg)))

(defn- time-of-next-external-update [pkg]
  (or (pq/peek-key (:input pkg)) infinity))

(defn time-of-next-update [pkg]
  (min (time-of-next-internal-update pkg)
       (time-of-next-external-update pkg)))

(defn- int-update% [pkg t]
  (let [sim             (:sim pkg)
        [sim' out-msg*] (int-update sim t)
        pkg'            (assoc pkg :sim sim')]
    [pkg' out-msg*]))

(defn- ext-update% [pkg t]
  (let [sim     (:sim pkg)
        input   (:input pkg)
        in-msg* (pq/peek input)
        sim'    (ext-update sim in-msg* t)
        pkg'    (-> pkg
                    (assoc :sim sim')
                    (update :input pq/pop))]
    [pkg' nil]))

(defn- con-update% [pkg t]
  (let [sim             (:sim pkg)
        input           (:input pkg)
        in-msg*         (pq/peek input)
        [sim' out-msg*] (con-update sim in-msg* t)
        pkg'            (-> pkg
                            (assoc :sim sim')
                            (update :input pq/pop))]
    [pkg' out-msg*]))

(defn step
  "Advance the simulation past the next event.

  Returns [new-pkg output-messages].

  All output messages occur at (time-of-last-update new-pkg), and are otherwise
  unordered."
  [pkg]
  (let [int-tn (time-of-next-internal-update pkg)
        ext-tn (time-of-next-external-update pkg)]
    (cond
      ;; No pending events.
      (and (= int-tn infinity)
           (= ext-tn infinity)) [pkg nil]
      ;; The sim internal update is next.
      (< int-tn ext-tn)         (int-update% pkg int-tn)
      ;; An external message is next.
      (< ext-tn int-tn)         (ext-update% pkg ext-tn)
      ;; The sim internal update is at the same time as the next external message.
      :else                     (con-update% pkg int-tn))))

(defn schedule
  "Schedule msg at time t."
  [pkg t msg]
  (when (< t (tl (:sim pkg)))
    (throw (ex-info "Cannot schedule event before last update time."
                    {:t   t
                     :msg msg
                     :tl  (tl (:sim pkg))})))
  (update pkg :input pq/insert t msg))

(defn schedule*
  "Schedule messages. tmsgs is a seq of [t msgs]."
  [pkg tmsgs]
  (reduce (fn [pkg [t msg]]
            (schedule pkg t msg))
          pkg
          (for [[t msgs] tmsgs
                msg      msgs]
            [t msg])))
