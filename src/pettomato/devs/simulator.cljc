(ns pettomato.devs.simulator)

(defprotocol Simulator
  (initialize         [sim t]
    "Initialize sim at current sim-time t. Returns sim.")
  (collect-mail       [sim t] ; Returns sim, b/c sim may update state to store local mail.
    "Get outgoing mail for sim at current sim-time t. Returns [sim mail],
where mail = p->vs.")
  (transition         [sim mail t]
    "Execute a state transition, where mail = p->vs and t is the current
sim-time. Returns [sim mail], where mail = p->vs.")
  (time-of-last-event [sim]
    "Returns the last time sim was updated.")
  (time-of-next-event [sim]
    "Returns the scheduled time of the next sim internal update, i.e., the maximum
time sim will remain in its current state, i.e., the time of sim's next update
if it doesn't receive any external messages before then."))
