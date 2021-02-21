(ns pettomato.devs.lib.clock
  "A simulation clock based on wall time. Most functions take the current wall
  time as an argument, which must be nondecreasing. Use a consistent and
  reliable source for supplying wall time, such as `(.getTime (java.util.Date.))`.")

;; Other than mapping wall-time to sim-time, most of the complexity comes from
;; handling pausing, which requires checkpointing wall-time and sim-time.

(defn clock
  "Returns a new clock.

  Args:

    wall-time - Current wall-time. A number.

    sim-time - Initial sim-time. A number.

  Keyword args:

    scale-factor - Scale factor. A number. Default: 1.0.

    paused? - If true, the simulation clock will be paused. Default: false.

  A scale factor of 2, will cause the sim-time to advance by 2 seconds for every
  1 second of wall-time; in other words, sim-time will advance twice as fast as
  wall-time. A scale factor of 0.5 will cause the sim-time to advance by 1
  second for every 2 seconds of wall-time; half as fast. A scale factor of -1
  will cause sim-time to advance towards negative infinity; i.e., in reverse."
  [wall-time sim-time & {:keys [scale-factor paused?]
                         :or   {scale-factor 1.0
                                paused?      false}}]
  {:wall-time    wall-time
   :sim-time     sim-time
   :scale-factor scale-factor
   :paused?      paused?})


(defn advance
  "Advance sim-time based on the current wall-time. Throws an exception if
  wall-time is less than the previously supplied wall-time. Returns a new
  clock."
  [clock wall-time]
  (if (:paused? clock)
    (assoc clock :wall-time wall-time) ;; wall-time is preserved to enforce the nondecreasing invariant.
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
                                 sim-time-delta  (* wall-time-delta (:scale-factor clock))
                                 prev-sim-time   (:sim-time clock)
                                 curr-sim-time   (long (+ prev-sim-time sim-time-delta))]
                             (assoc clock
                                    :wall-time curr-wall-time
                                    :sim-time  curr-sim-time))))))

(defn get-sim-time
  "Returns the current simulation time.

  Note that this does not advance the simulation automatically. Typically, you
  will want to call `advance` prior to calling this function."
  [clock]
  (:sim-time clock))


(defn pause
  "Pause the simulation clock."
  [clock wall-time]
  (-> clock
      (advance wall-time)
      (assoc :paused? true)))

(defn unpause
  "Unpause the simulation clock."
  [clock wall-time]
  (-> clock
      (assoc :wall-time wall-time)
      (assoc :paused? false)))

(defn paused?
  "Returns true if clock is paused, otherwise false."
  [clock]
  (:paused? clock))


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
