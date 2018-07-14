(ns pettomato.devs.Simulator)

(defprotocol Simulator
  (init       [sim t]   "Returns an instance of an implementation of Simulator.")
  (int-update [sim t]   "Triggers an internal update of the simulation. Returns [new-sim ouput-messages].")
  (ext-update [sim x t] "Triggers an external update of the simulation. Returns new-sim.")
  (con-update [sim x t] "Triggers a confluent update of the simulation. Returns [new-sim output-messages].")
  (tl         [sim]     "Time of the last sim update.")
  (tn         [sim]     "Time of the next internal sim update."))
