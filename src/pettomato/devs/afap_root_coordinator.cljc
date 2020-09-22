(ns pettomato.devs.afap-root-coordinator
  "A root coordinator that executes a simulation as fast as possible.

  \"Analytic simulations typically execute 'as-fast-as-possible,' meaning that
  the simulation attempts to complete its computations as quickly as it
  can. This could mean that the simulator advances faster than real-time (for
  example, it might simulate hours of system behavior in only minutes of elapsed
  time to the user) or that it runs slower than real-time.\"

    - Fujimoto. Parallel and Distributed Simulation Systems. 2000. p. 7."
  (:require
   [pettomato.devs.Simulator :refer [receive-i-message
                                     receive-*-message
                                     receive-x-message
                                     time-of-next-event]]))

(defn afap-root-coordinator [sim start-time end-time]
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

(defn lazy-afap-root-coordinator [sim start-time end-time]
  (letfn [(step [sim]
            (let [t (time-of-next-event sim)]
              (if (< end-time t)
                nil
                (let [[sim' out'] (receive-*-message sim t)
                      sim'        (receive-x-message sim' {} t)]
                  (if (seq out')
                    (cons [t out'] (lazy-seq (step sim')))
                    (lazy-seq (step sim')))))))]
    (lazy-seq (step (receive-i-message sim start-time)))))
