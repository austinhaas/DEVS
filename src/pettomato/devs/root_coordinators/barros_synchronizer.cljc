(ns pettomato.devs.root-coordinators.barros-synchronizer
  "Barros. Abstract Simulators for the DSDE Formalism. 1998.
   https://repository.lib.ncsu.edu/bitstream/handle/1840.4/6989/1998_0056.pdf"
  (:require
   [pettomato.devs.Simulator :refer [receive-i-message
                                     receive-*-message
                                     receive-x-message
                                     time-of-next-event]]))

(defn synchronizer [sim start-time end-time]
  (loop [sim (receive-i-message sim start-time)
         out []]
    (let [t (time-of-next-event sim)]
      (if (< end-time t)
        out
        (let [[sim' out'] (receive-*-message sim t)
              sim'        (receive-x-message sim' {} t)]
          (recur sim' (if (seq out')
                        (conj out [t out'])
                        out)))))))
