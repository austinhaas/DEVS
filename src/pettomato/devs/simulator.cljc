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

  Returns [sim mail].

  Throws an exception if:

    - t <= time-of-last-event.
    - t > time-of-next-event.
    - time-of-next-event is infinite.
    - t < time-of-next-event and mail is empty.

  This is a low-level function. `step*` is a higher-level function
  with a friendlier interface."
  ([sim]
   (let [tn (time-of-next-event sim)]
     (ex-assert (not (h/infinite? tn))
                "Cannot step sim, because time-of-next-event is infinite.")
     (let [mail (collect-mail sim tn)
           sim  (transition sim {} tn)]
       [sim mail])))
  ([sim t mail]
   (let [tl (time-of-last-event sim)
         tn (time-of-next-event sim)]
     (ex-assert (h/< tl t) "Synchronization error.")
     (ex-assert (h/<= t tn) "Synchronization error.")
     (ex-assert (or (h/= t tn)
                    (seq mail))
                "Sim must be imminent or receiving mail.")
     (let [mail' (if (h/= t tn)
                   (collect-mail sim t)
                   {})
           sim   (transition sim mail t)]
       [sim mail']))))

(defn step*
  "Repeatedly step sim. Returns a lazy seq of [sim mail].

  Options:

   end - Simulation end time (inclusive, unless  infinity).
   Default: (hyperreal) infinity.

   mail-log - A (possibly lazy) seq of [time mail] that will be sent
   to sim at the appropriate times.

  The sequence terminates when there are no more states; that is, when
  t is reached or time-of-next-event is infinite."
  ;; Implementation note: In an attempt to decompose this further, I
  ;; considered discarding parameter `t`, and instead treating this
  ;; like an output stream (i.e., of [sim mail]) that can be merged
  ;; with an input stream (i.e., mail-log) to produce a new output
  ;; stream, but there would be no simple way to factor in `t` after
  ;; the streams have been merged, because there would be no access to
  ;; mail-log. For example, a reactive sim that is passive until it
  ;; receives input could have a time-of-next-event value of infinity,
  ;; but at the same time there could be outstanding events remaining
  ;; in the mail-log, and there's no way to determine when they occur
  ;; by inspecting the output stream alone.
  ([sim & {:keys [end mail-log]
           :or   {end      h/infinity
                  mail-log []}}]
   (lazy-seq
    (let [tn         (time-of-next-event sim)
          [mtn mail] (first mail-log)]
      (cond
        (and (seq mail-log)
             (h/<= mtn tn)
             (h/<= mtn end)) (let [[sim mail] (step sim mtn mail)]
                               (cons [sim mail] (step* sim :end end :mail-log (rest mail-log))))
        (h/infinite? tn)     []
        (h/<= tn end)        (let [[sim mail] (step sim)]
                               (cons [sim mail] (step* sim :end end :mail-log mail-log)))
        :else                [])))))

(defn output->mail-log [xs]
  (->> xs
       (remove (comp empty? second))
       (map (juxt (comp time-of-last-event first)
                  second))))
