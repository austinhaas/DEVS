(ns pettomato.devs.rt-simulation-clock
  "This ns provides a means to manage a simulation clock that is pegged
  to wall time. Most functions take the current wall time as an
  argument, which must always be nondecreasing. Users should use a
  consistent and reliable source for supplying wall time, such
  as `(.getTime (java.util.Date.))`.")

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
  "Advances sim time based on current wall time wt. Throws an exception
  if wt is less than the previously supplied wt for clock."
  [clock wt]
  (assert (<= (:wt clock) wt))
  (-> clock
      (assoc :wt wt)
      (update :st + (* (- wt (:wt clock)) (:scale clock)))))

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

(defn advance [clock wt]
  (if (:paused clock)
    (assoc clock :wt wt)
    (advance% clock wt)))

(defn get-time
  "Returns the current simulation time."
  [clock wt]
  (:st (advance clock wt)))

(defn paused? [clock] (:paused clock))

#_
(-> (clock 0 0 0.5)
    (advance 10)
    (pause 12)
    (advance 15)
    (unpause 16)
    (set-scale 18 -2.0)
    (advance 20)
    )
