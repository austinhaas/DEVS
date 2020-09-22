(ns pettomato.devs.Simulator)

;; This implementation deviates from the book in a functional programming
;; style. Specifically, receive-*-message returns output, rather than sending a
;; y-message to its parent.

(defprotocol Simulator
  (receive-i-message  [sim t]   "Initialization. Returns new sim.")
  (receive-*-message  [sim t]   "Compute output. Returns [sim mail].")
  (receive-x-message  [sim x t] "Execute a state transition. Returns new sim.")
  (time-of-last-event [sim]     "Time of the last sim update.")
  (time-of-next-event [sim]     "Scheduled time of next sim internal update."))
