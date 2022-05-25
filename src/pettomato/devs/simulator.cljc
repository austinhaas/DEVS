(ns pettomato.devs.simulator
  (:require
   [pettomato.devs.lib.coll :refer [take-until]]
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
  "Advance sim to the time of its next event. Returns [sim
  mail]. Returns nil if (time-of-next-event sim) is infinite."
  [sim]
  (let [tn (time-of-next-event sim)]
    (if (h/infinite? tn)
      nil
      (let [mail (collect-mail sim tn)
            sim  (transition sim {} tn)]
        [sim mail]))))

(defn step*
  "Repeatedly step sim. Returns a lazy seq of [sim mail].

  The sequence terminates when (time-of-next-event sim) is infinite or
  greater than `end`."
  [sim & {:keys [end]
          :or   {end h/infinity}}]
  (let [done? (fn [sim]
                (let [tn (time-of-next-event sim)]
                  (or (h/infinite? tn)
                      (h/< end tn))))]
   (->> (iterate (comp step first) [sim nil])
        (take-until (comp done? first))
        (drop 1))))

(defn insert [sim mail t]
  (let [tl (time-of-last-event sim)
        tn (time-of-next-event sim)]
    (ex-assert (h/< tl t))
    (ex-assert (h/<= t tn))
    (let [mail' (if (h/= t tn)
                  (collect-mail sim t)
                  {})
          sim  (transition sim mail t)]
      [sim mail'])))
