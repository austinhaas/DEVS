(ns des.Simulator)

(defprotocol Simulator
  (init       [this t])
  (int-update [this t])
  (ext-update [this x t])
  (tl         [this])
  (tn         [this]))
