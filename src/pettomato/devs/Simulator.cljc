(ns pettomato.devs.Simulator)

(defprotocol Simulator
  (init       [sim t])
  (int-update [sim t])
  (ext-update [sim x t])
  (tl         [sim])
  (tn         [sim]))
