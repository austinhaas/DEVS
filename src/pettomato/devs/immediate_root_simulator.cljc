(ns pettomato.devs.immediate-root-simulator
  (:require
   [pettomato.devs.root-simulator-base :as rs]))

(defn immediate-root-simulator
  [sim sim-start-time sim-end-time tmsg*]
  (-> (rs/root-simulator sim sim-start-time)
      (rs/schedule* tmsg*)
      (rs/advance sim-end-time)
      second))
