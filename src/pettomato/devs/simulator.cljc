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
    - t is not provided and time-of-next-event is infinite.
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
  "Repeatedly step sim. Returns [sim mail-log].

  If `t` is provided, the sim will not be stepped past t.

  If `t` and `mail` are provided, the sim will stepped up to `t` and
  then at `t` the sim will be stepped with `mail` as input."
  ;; TODO: Add additional optional parameters like sim-end-fn and
  ;; mail-end-fn that cause the sim to terminate when they return
  ;; true. We might want to terminate when a particular message is
  ;; returned, for example.

  ;; Implementation note: This function was defined with multiple
  ;; arities instead of optional keywords to enforce the only
  ;; meaningful use cases. For example, if `mail` could be supplied
  ;; without `t`, that would mean, send the mail whenever the next
  ;; internal event occurs, and I can't imagine a scenario where that
  ;; would be a good idea.

  ([sim] ; Step until sim's time-of-next-event is infinite.
   (step* sim h/infinity))
  ([sim t] ; Step until sim's time-of-next-event is > t.
   (loop [sim      sim
          mail-log []]
     (let [tn (time-of-next-event sim)]
       (if (or (h/infinite? tn)
               (h/< t tn))
         [sim mail-log]
         (let [[sim mail] (step sim)
               mail-log   (if (seq mail)
                            (conj mail-log [(time-of-last-event sim) mail])
                            mail-log)]
           (recur sim mail-log))))))
  ([sim t mail] ; Step to t and send mail.
   (loop [sim      sim
          mail-log []]
     (let [tn (time-of-next-event sim)]
       (if (or (h/infinite? tn)
               (h/<= t tn))
         (let [[sim mail] (step sim t mail)
               mail-log   (if (seq mail)
                            (conj mail-log [(time-of-last-event sim) mail])
                            mail-log)]
           [sim mail-log])
         (let [[sim mail] (step sim)
               mail-log   (if (seq mail)
                            (conj mail-log [(time-of-last-event sim) mail])
                            mail-log)]
           (recur sim mail-log)))))))

(defn step**
  "Repeatedly step sim and incorporate a mail-log of mail to send to sim
  at the designated times.

  Returns [sim mail-log].

  Optional parameters:

    end - Simulation end time (inclusive, unless infinity).
          Default: (hyperreal) infinity.

    mail-log - A seq of [time mail].

  Prefer `step*`, unless you really need to send multiple mail
  messages in one call."
  [sim & {:keys [end mail-log]
          :or   {end      h/infinity
                 mail-log []}}]
  (loop [sim        sim
         input-log  (take-while #(h/<= (first %) end) mail-log)
         output-log []]
    (if (seq input-log)
      (let [[t mail]       (first input-log)
            [sim mail-log] (step* sim t mail)]
        (recur sim (rest input-log) (into output-log mail-log)))
      (let [[sim mail-log] (step* sim end)]
        [sim (into output-log mail-log)]))))
