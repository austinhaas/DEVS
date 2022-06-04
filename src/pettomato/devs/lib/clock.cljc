(ns pettomato.devs.lib.clock
  "A simulation clock that is paced by wall-time.

  Supports time scaling, e.g., to run a simulation at double speed.

  Most functions take wall-time as an argument. Wall-time must be
  nondecreasing. Choose a consistent and reliable source, such
  as `(.getTime (java.util.Date.))`.

  Wall-time isn't managed internally for two reasons:

  1. Some host platform APIs that can be used to drive this (e.g.,
  requestAnimationFrame, for ClojureScript) provide an integer that
  represents the elapsed time. It would be convoluted to implement
  anything more complicated than taking that value as a parameter.

  2. You may want more control over the way time advances, rather than
  getting wall-time indiscriminately on every function call. For
  example, you might want to get wall-time once, at the top of an
  animation frame, and then call several API fns with that value."
  (:require
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.hyperreal :as h]))

;; This implementation is complex because scale-factor can change over
;; time, which requires recording checkpoint values for wall-time and
;; sim-time.

(defn clock
  "Returns a new simulation clock.

  Args:

    wall-time - Initial wall-time. A number.

    sim-time - Initial sim-time. A hyperreal number.

  Keyword args:

    scale-factor - Scale factor. A number. Default: 1.0.

  A scale factor of 2, will cause the sim-time to advance by 2 seconds for every
  1 second of wall-time; in other words, sim-time will advance twice as fast as
  wall-time. A scale factor of 0.5 will cause the sim-time to advance by 1
  second for every 2 seconds of wall-time; half as fast. A scale factor of -1
  will cause sim-time to advance towards negative infinity; i.e., in reverse."
  [wall-time sim-time & {:keys [scale-factor]
                         :or   {scale-factor 1.0}}]
  (ex-assert (h/hyperreal? sim-time))
  {:wall-time    wall-time
   :sim-time     sim-time
   :scale-factor scale-factor})

(defn set-wall-time
  "Set wall-time, and consequently sim-time. Returns new clock.

  Throws an exception if wall-time is less than the previously
  supplied wall-time. Returns a new clock."
  [clock wall-time]
  (let [curr-wall-time wall-time
        prev-wall-time (:wall-time clock)]
    (cond
      (< curr-wall-time
         prev-wall-time) (throw (ex-info "The current wall-time cannot be less than the previously supplied wall-time."
                                         {:curr-wall-time curr-wall-time
                                          :prev-wall-time prev-wall-time}))
      (= curr-wall-time
         prev-wall-time) clock
      :else              (let [wall-time-delta (- curr-wall-time prev-wall-time)
                               sim-time-delta  (h/*R (* wall-time-delta (:scale-factor clock)))
                               prev-sim-time   (:sim-time clock)
                               curr-sim-time   (h/+ prev-sim-time sim-time-delta)]
                           (assoc clock
                                  :wall-time curr-wall-time
                                  :sim-time  curr-sim-time)))))

(defn get-sim-time
  "Returns the current simulation time.

  Call `set-wall-time` to advance the clock to the current time before
  calling this function, or pass wall-time as an argument."
  ([clock]
   (:sim-time clock))
  ([clock wall-time]
   (:sim-time (set-wall-time clock wall-time))))

(defn get-scale-factor
  "Returns clock's scale factor."
  [clock]
  (:scale-factor clock))

(defn set-scale-factor
  "Set clock's scale factor.

  Call `set-wall-time` to advance the clock to the current time before
  calling this function, or pass wall-time as an argument.

  This can be used to pause/unpause the simulation."
  ([clock scale-factor]
   (-> clock
       (assoc :scale-factor scale-factor)))
  ([clock wall-time scale-factor]
   (-> clock
       (set-wall-time wall-time)
       (assoc :scale-factor scale-factor))))
