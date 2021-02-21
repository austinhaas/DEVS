(ns pettomato.devs.simulator)

(defprotocol Simulator
  (initialize         [sim t]      "Initialize sim at current sim-time t. Returns sim.")
  (collect-mail       [sim t]      "Get outgoing mail for sim at current sim-time t. Returns [sim mail], where mail = p->vs.") ; Returns sim, b/c sim may update state based on local mail.
  (transition         [sim mail t] "Execute a state transition, where mail = p->vs and t is the current sim-time. Returns sim.")
  (time-of-last-event [sim]        "Returns the last time sim was updated.")
  (time-of-next-event [sim]        "Returns the scheduled time of the next sim internal update."))
