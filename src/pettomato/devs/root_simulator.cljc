(ns pettomato.devs.root-simulator
  "A root-coordinator."
  (:require
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.priority-queue :as pq]
   [pettomato.devs.Simulator :refer [init int-update ext-update con-update tl tn]]))

(defn root-simulator [sim start-time]
  [(init sim start-time) (pq/init)])

(defn advance
  "Advances the simulator no later than max-time.

   Returns an updated root and a collection of timestamped messages,
  sorted by timestamp, that the simulator outputs during the update.

   The simulator records the last time that it was updated and will
  throw errors if an attempt is made to update it before that
  time. This function relies on that behavior, and does not perform
  additional error-checking.

   max-time is an upper-bound for the step. It is inaccurate to say
  that the sim is updated to max-time. The sim's internal clock is set
  to the time of the last event, which may have occurred some time
  before max-time. There may be valid messages received after this
  update has run, but between the sim's internal time and
  max-time. The sim is always advanced from the the time of its last
  event, so those messages will be processed the next time advance is
  called. Because this is a discrete-event simulation, there is no
  cost to repeatedly processing the same interval of time if nothing
  occurs during that time.

   One caveat to the above remark, regarding confluence: it is
  possible to schedule an internal update at max-time, then call
  advance, and then schedule an event at max-time. The internal and
  external event should be handled by the confluent function, but in
  this case they won't. If that is possible, for example, when using a
  real-time system, then the client should fix that issue. One
  solution is to decrement max-time by 1 before calling this function,
  so that any undiscovered events will always be after max-time."
  [root max-time]
  (let [[sim pq] root]
    (loop [sim      sim
           tmsg-in  pq
           tmsg-out (transient [])]
      ;; int-tn - The time of the next internal update.
      ;; ext-tn - The time of the next external update.
      (let [int-tn (tn sim)
            ext-tn (or (pq/peek-key tmsg-in) infinity)]
        (cond
          ;; No more events before max-time.
          ;; Return the updated sim and any messages it output.
          (and (or (< max-time int-tn)
                   (= int-tn infinity))
               (or (< max-time ext-tn)
                   (= ext-tn infinity))) [[sim tmsg-in] (persistent! tmsg-out)]
          ;; The sim internal update is next.
          (< int-tn ext-tn)              (let [[sim' msg*] (int-update sim int-tn)]
                                           (recur sim'
                                                  tmsg-in
                                                  (if (seq msg*)
                                                    (conj! tmsg-out [int-tn msg*])
                                                    tmsg-out)))
          ;; An external message is next.
          (< ext-tn int-tn)              (let [t    (pq/peek-key tmsg-in)
                                               msg* (pq/peek tmsg-in)]
                                           (recur (ext-update sim msg* t)
                                                  (pq/pop tmsg-in)
                                                  tmsg-out))
          ;; The sim internal update is at the same time as the next external message.
          :else                          (let [t            (pq/peek-key tmsg-in)
                                               msg*         (pq/peek tmsg-in)
                                               [sim' msg*'] (con-update sim msg* t)]
                                           (recur sim'
                                                  (pq/pop tmsg-in)
                                                  (if (seq msg*')
                                                    (conj! tmsg-out [int-tn msg*'])
                                                    tmsg-out))))))))

(defn schedule [root t msg]
  (let [[sim' pq] root]
    [sim' (pq/insert pq t msg)]))

(defn schedule* [root tmsgs]
  (reduce (fn [root [t msg]] (schedule root t msg))
          root
          tmsgs))
