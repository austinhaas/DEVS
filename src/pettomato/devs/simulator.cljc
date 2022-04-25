(ns pettomato.devs.simulator)

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
