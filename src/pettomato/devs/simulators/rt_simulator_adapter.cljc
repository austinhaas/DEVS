(ns pettomato.devs.simulators.rt-simulator-adapter
  "Convert a non-real-time simulator to a real-time simulator."
  (:require
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [Simulator initialize collect-mail transition time-of-last-event time-of-next-event]]))

;; This should probably cache time-of-next-event, to avoid calling down to the
;; wrapped simulator more than would be normal for a non-real-time simulator.

(defrecord RTSimulatorAdapter [sim]
  Simulator
  (initialize [sim' t]
    (update sim' :sim initialize t))
  (collect-mail [sim' t]
    (log/tracef "rt-simulator-adapter/collect-mail")
    (let [[sim out] (collect-mail sim t)]
      [(assoc sim' :sim sim) out]))
  (transition [sim' ext-mail t]
    (log/tracef "rt-simulator-adapter/transition mail: %s tn: %s"
                (count (seq ext-mail)) (time-of-next-event sim))
    (if (or (seq ext-mail)
            (= t (time-of-next-event sim)))
      (update sim' :sim transition ext-mail t)
      sim'))
  (time-of-last-event [sim']
    (time-of-last-event sim))
  (time-of-next-event [sim']
    (time-of-next-event sim)))

(defn rt-simulator-adapter
  "Takes a non-real-time simulator and converts it into a real-time simulator."
  [sim]
  (RTSimulatorAdapter. sim))
