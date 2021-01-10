(ns pettomato.devs.lib.clock
  "This ns provides a means to manage a simulation clock that is based on wall
  time. Most functions take the current wall time as an argument, which must
  always be nondecreasing. Users should use a consistent and reliable source for
  supplying wall time, such as `(.getTime (java.util.Date.))`.")

(defn clock
  "Initializes and returns a clock data structure.

    wt - The current wall time. A number.

    st - The initial sim time. A number.

    scale - The scale factor. A number. For example, if the scale
  factor is 0.5, then for every 2 seconds of wall time, sim time will
  advance by 1 second. May be negative."
  [wt st scale]
  {:wt     wt
   :st     st
   :scale  scale
   :paused true})

(defn- advance%
  "Advances sim time based on current wall time wt. Throws an exception if wt is
  less than the previously supplied wall time for clock. Returns a new advanced
  clock."
  [clock wt]
  (let [wt-delta (- wt (:wt clock))]
    (assert (<= 0 wt-delta))
    (cond
      (neg? wt-delta)  (throw (ex-info "Cannot advance% if wt is less than previous wall-time" {:wt wt :prev (:wt clock)}))
      (zero? wt-delta) clock
      :else            (let [st-delta (* wt-delta (:scale clock))
                             st       (long (+ (:st clock) st-delta))]
                         (assoc clock :wt wt :st st)))))

(defn set-time [clock wt st]
  (assoc clock :wt wt :st st))

(defn get-scale [clock]
  (:scale clock))

(defn set-scale [clock wt scale]
  (-> clock
      (advance% wt)
      (assoc :scale scale)))

(defn pause [clock wt]
  (-> clock
      (advance% wt)
      (assoc :paused true)))

(defn unpause [clock wt]
  (-> clock
      ;; Sim time is not advanced!
      (assoc :wt wt)
      (assoc :paused false)))

(defn paused? [clock] (:paused clock))

(defn advance [clock wt]
  (if (:paused clock)
    (assoc clock :wt wt)
    (advance% clock wt)))

(defn get-time
  "Returns the current simulation time."
  [clock wt]
  (:st (advance clock wt)))
