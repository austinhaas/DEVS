(ns pettomato.devs.lib.clock
  "A simulation clock that is paced by wall-time.

  Supports pausing and time scaling, e.g., to run a simulation at
  double speed."
  (:require
   [pettomato.devs.lib.date :as date]
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.hyperreal :as h]))

;; This implementation is complex because scale-factor can change over
;; time, which requires recording checkpoint values for wall-time and
;; sim-time.

(defn clock
  "Returns a new simulation clock.

  Keyword args:

    sim-time - Initial sim-time. A hyperreal
  number. Default: (hyperreal) zero.

    wall-time-fn - A function of no args that returns the current
  wall-time. See `pettomato.devs.lib.clock` for more info.

    paused? - Is the clock initially paused? A boolean. Default:
  false.

    scale-factor - Scale factor. A number. Default: 1.0.

  A scale factor of 2, will cause the sim-time to advance by 2 seconds
  for every 1 second of wall-time; in other words, sim-time will
  advance twice as fast as wall-time. A scale factor of 0.5 will cause
  the sim-time to advance by 1 second for every 2 seconds of
  wall-time; half as fast. A scale factor of -1 will cause sim-time to
  advance towards negative infinity; i.e., in reverse."
  [& {:keys [sim-time wall-time-fn paused? scale-factor]
      :or   {sim-time     h/zero
             wall-time-fn date/timestamp
             paused?      false
             scale-factor 1.0}}]
  (ex-assert (h/hyperreal? sim-time))
  (ex-assert (fn? wall-time-fn))
  (ex-assert (boolean? paused?))
  (ex-assert (number? scale-factor))
  {:wall-time    (wall-time-fn)
   :sim-time     sim-time
   :wall-time-fn wall-time-fn
   :paused?      paused?
   :scale-factor scale-factor})

(defn paused?
  "Returns true if the clock is paused, otherwise false."
  [clock]
  (:paused? clock))

(defn- set-wall-time
  "Set wall-time, and consequently sim-time. Returns new clock.

  Throws an exception if wall-time is less than the previously
  supplied wall-time. Returns a new clock."
  [clock]
  (let [curr-wall-time ((:wall-time-fn clock))
        prev-wall-time (:wall-time clock)]
    (cond
      (< curr-wall-time
         prev-wall-time) (throw (ex-info "The current wall-time cannot be less than the previously supplied wall-time."
                                         {:curr-wall-time curr-wall-time
                                          :prev-wall-time prev-wall-time}))
      (= curr-wall-time
         prev-wall-time) clock
      (paused? clock)    (assoc clock :wall-time curr-wall-time)
      :else              (let [wall-time-delta (- curr-wall-time prev-wall-time)
                               sim-time-delta  (h/*R (* wall-time-delta (:scale-factor clock)))
                               prev-sim-time   (:sim-time clock)
                               curr-sim-time   (h/+ prev-sim-time sim-time-delta)]
                           (assoc clock
                                  :wall-time curr-wall-time
                                  :sim-time  curr-sim-time)))))

(defn pause
  "Pause the clock."
  [clock]
  (-> clock
      set-wall-time
      (assoc :paused? true)))

(defn unpause
  "Unpause the clock."
  [clock]
  (-> clock
      set-wall-time
      (assoc :paused? false)))

(defn get-sim-time
  "Returns the current simulation time."
  [clock]
  (:sim-time (set-wall-time clock)))

(defn get-scale-factor
  "Returns the clock's scale factor."
  [clock]
  (:scale-factor clock))

(defn set-scale-factor
  "Set the clock's scale factor."
  [clock scale-factor]
  (-> clock
      set-wall-time
      (assoc :scale-factor scale-factor)))
