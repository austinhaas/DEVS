(ns pettomato.devs.immediate-system
  (:require
   [pettomato.devs.simulation :refer [advance]]
   [pettomato.devs.Simulator :refer [init]]))

(defn immediate-system
  "A system that runs the simulation from start-time up to max-time as
  fast as possible. In other words, this is not a real-time system."
  ([sim start-time max-time] (immediate-system sim start-time max-time []))
  ([sim start-time max-time tmsg-in]
   (-> (advance (init sim start-time) max-time tmsg-in)
       second)))
