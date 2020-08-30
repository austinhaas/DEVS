(ns pettomato.devs.afap-root-simulator
  "As fast as possible. Runs a simulator from start to finish as fast as the host
  computer can compute it."
  (:require
   [pettomato.devs.root-simulator-base :as rsb]))

(defn- step-until [root-sim sim-end-time output-messages]
  (let [tn (rsb/time-of-next-update root-sim)]
   (if (< tn sim-end-time)
     (let [[root-sim' messages] (rsb/step root-sim)]
       (recur root-sim'
              sim-end-time
              (if (seq messages)
                (conj output-messages [tn messages])
                output-messages)))
     [root-sim output-messages])))

(defn afap-root-simulator
  [sim sim-start-time sim-end-time tmsg*]
  (-> (rsb/root-simulator sim sim-start-time)
      (rsb/schedule* tmsg*)
      (step-until sim-end-time [])))
