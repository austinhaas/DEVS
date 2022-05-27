(ns pettomato.devs.simulator
  (:require
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.hyperreal :as h]))

(defprotocol Simulator
  (initialize         [sim t]
    "Initialize sim at sim-time t. Returns a new sim.")
  (collect-mail       [sim t]
    "Get outgoing mail for sim at sim-time t. Returns mail,
where mail = p->vs.")
  (transition         [sim mail t]
    "Execute a state transition at sim-time t, with mail = p->vs. Returns
a new sim.")
  (time-of-last-event [sim]
    "Returns the last time sim was updated.")
  (time-of-next-event [sim]
    "Returns the scheduled time of the next sim internal update, i.e., the maximum
time sim will remain in its current state, i.e., the time of sim's next update
if it doesn't receive any external messages before then."))

(defn step
  "Advance sim to time t and send it mail at that time.

  If only sim is provided, advance sim to the time of its next event.

  Returns [sim mail], or nil if if sim cannot be advanced because the
  time of the next event is infinite.

  Throws an exception if t <= time-of-last-event or if t >
  time-of-next-event."
  ([sim]
   (let [tn (time-of-next-event sim)]
     (if (h/infinite? tn)
       nil
       (let [mail (collect-mail sim tn)
             sim  (transition sim {} tn)]
         [sim mail]))))
  ([sim t mail]
   (let [tl (time-of-last-event sim)
         tn (time-of-next-event sim)]
     (ex-assert (h/< tl t) "Synchronization error.")
     (ex-assert (h/<= t tn) "Synchronization error.")
     (ex-assert (or (h/= t tn)
                    (seq mail)))
     (let [mail' (if (h/= t tn)
                   (collect-mail sim t)
                   {})
           sim   (transition sim mail t)]
       [sim mail']))))

(defn step*
  "Repeatedly step sim. Returns a lazy seq of [sim mail].

  If mail is provided, sends mail to sim at time t.

  The sequence terminates when the time of the next event is infinite
  or greater than t."
  ([sim]
   (step* sim h/infinity {}))
  ([sim t]
   (step* sim t {}))
  ([sim t mail]
   (lazy-seq
    (let [tl (time-of-last-event sim)
          tn (time-of-next-event sim)]
      (ex-assert (h/< tl t) "Synchronization error.")
      (cond
        (and (h/infinite? t)
             (h/infinite? tn)) []
        (h/< tn t)             (let [[sim mail'] (step sim)]
                                 (cons [sim mail'] (step* sim t mail)))
        (h/= t tn)             [(step sim t mail)]
        (seq mail)             [(step sim t mail)]
        :else                  [])))))

(defn output->mail-log [xs]
  (->> xs
       (remove (comp empty? second))
       (map (juxt (comp time-of-last-event first)
                  second))))
