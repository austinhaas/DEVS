(ns pettomato.devs.simulator)

(defprotocol Simulator
  (initialize         [sim t]      "Initialize sim. Returns sim.")
  (collect-mail       [sim t]      "Returns [sim mail]. mail = p->vs.") ; Returns sim, b/c sim may update state based on local mail.
  (transition         [sim mail t] "mail = p->vs. Returns sim.")
  (time-of-last-event [sim]        "Returns the time of the last sim update.")
  (time-of-next-event [sim]        "Returns the scheduled time of the next sim internal update."))
