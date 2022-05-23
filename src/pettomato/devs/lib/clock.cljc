(ns pettomato.devs.lib.clock
  "A simulation clock based on wall time. Some functions take the current wall
  time as an argument, which must be nondecreasing. Use a consistent and
  reliable source for supplying wall time, such as `(.getTime (java.util.Date.))`."
  (:require
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.hyperreal :as h]))

;; This implementation is complex because scale-factor can change over
;; time, which requires recording checkpoint values for wall-time and
;; sim-time.

(defn clock
  "Returns a new clock.

  Args:

    wall-time - Current wall-time. A number.

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

(defn advance
  "Advance sim-time based on the current wall-time. Throws an exception if
  wall-time is less than the previously supplied wall-time. Returns a new
  clock."
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

  Note that this does not advance the simulation automatically. Typically, you
  will want to call `advance` prior to calling this function."
  [clock]
  (:sim-time clock))

(defn get-scale-factor
  "Returns clock's scale factor."
  [clock]
  (:scale-factor clock))

(defn set-scale-factor
  "Set clock's scale factor."
  [clock wall-time scale-factor]
  (-> clock
      (advance wall-time)
      (assoc :scale-factor scale-factor)))
