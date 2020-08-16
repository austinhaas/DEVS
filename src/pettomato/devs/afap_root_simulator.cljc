(ns pettomato.devs.afap-root-simulator
  "As fast as possible. Runs a simulation from start to finish as fast as the host
  computer can compute it."
  (:require
   [pettomato.devs.root-simulator-base :as rsb]))

(defn- step-until [root-sim sim-end-time]
  (if (< (rsb/time-of-next-update root-sim)
         sim-end-time)
    (recur (rsb/step root-sim) sim-end-time)
    root-sim))

(defn afap-root-simulator
  [sim sim-start-time sim-end-time tmsg*]
  (-> (rsb/root-simulator sim sim-start-time)
      (rsb/schedule* tmsg*)
      (step-until sim-end-time)))

(def output rsb/output)
(def clear-output rsb/clear-output)
