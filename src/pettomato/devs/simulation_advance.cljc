(ns pettomato.devs.simulation-advance
  (:require
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.Simulator :refer [int-update ext-update con-update tl tn]]))

(defn advance
  "Advances simulation sim no later than max-time.

   tmsg-in - (Optional.) A collection of timestamped messages
   (i.e., [timestamp messages]), sorted by timestamp, that will be
   passed to the simulation.

   Returns an updated sim and a collection of timestamped messages,
  sorted by timestamp, that the simulation outputs during the update.

   messages - A map from labels to collections of values. Each value
   is considered a message.

   For example, the timestamped messages

     [10 {:x [1 2] :y [3]}]

   represents 3 messages that occur at time 10:

     one with label :x and value 1,
     one with label :x and value 2, and
     one with label :y and value 3.

   The simulation records the last time that it was updated and will
  throw errors if an attempt is made to update it before that
  time. This function relies on that behavior, and does not perform
  additional error-checking, such as to make sure that tmsg-in is
  sorted.

   max-time is just an upper-bound for this update. It isn't accurate to
  say that the sim is updated to max-time. The sim's internal clock is
  set to the last time of an event, which may have occurred some time
  before max-time. There may be valid messages received after this
  update has run, but between the sim's internal time and max-time. The
  sim is always advanced from the time of it's last update, so those
  messages should be processed the next time advance is called. This is a
  discrete-event simulation, so there is no cost to repeatedly
  processing the same interval of time if nothing occurs during that
  time.

  An error is thrown if any messages in tmsg-in occur after
  max-time. This is considered preferable to silently dropping
  messages, and it mostly likely points out an error in the client
  code. However, I can imagine justification for making this behavior
  optional."
  ([sim max-time]
   (advance sim max-time []))
  ([sim max-time tmsg-in]
   (loop [sim      sim
          tmsg-in  tmsg-in
          tmsg-out (transient [])]
     ;; int-tn - The time of the next internal update.
     ;; ext-tn - The time of the next external update.
     (let [int-tn (tn sim)
           ext-tn (if (seq tmsg-in) (ffirst tmsg-in) infinity)]
       (cond
         ;; No more events before max-time.
         (and (or (< max-time int-tn)
                  (= int-tn infinity))
              (or (< max-time ext-tn)
                  (= ext-tn infinity))) (if (seq tmsg-in)
                                          #?(:clj  (throw (Exception. (str "These messages had timestamps greater than max-time=" max-time ": " tmsg-in)))
                                             :cljs (throw (js/Error.  (str "These messages had timestamps greater than max-time=" max-time ": " tmsg-in))))
                                          ;; Return the updated sim and any messages it output.
                                          [sim (persistent! tmsg-out)])
         ;; The sim internal update is next.
         (< int-tn ext-tn)              (let [[sim' msg*] (int-update sim int-tn)]
                                          (recur sim'
                                                 tmsg-in
                                                 (if (seq msg*)
                                                   (conj! tmsg-out [int-tn msg*])
                                                   tmsg-out)))
         ;; An external message is next.
         (< ext-tn int-tn)              (let [[t msg*] (first tmsg-in)]
                                          (recur (ext-update sim msg* t)
                                                 (rest tmsg-in)
                                                 tmsg-out))
         ;; The sim internal update is at the same time as the next external message.
         :else                          (let [[t msg*]     (first tmsg-in)
                                              [sim' msg*'] (con-update sim msg* t)]
                                          (recur sim'
                                                 (rest tmsg-in)
                                                 (if (seq msg*')
                                                   (conj! tmsg-out [int-tn msg*'])
                                                   tmsg-out))))))))
