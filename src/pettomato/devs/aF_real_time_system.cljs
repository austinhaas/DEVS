(ns pettomato.devs.aF-real-time-system
  (:require
   [pettomato.devs.util :refer [now]]
   [pettomato.devs.Simulator :refer [init tl]]
   [pettomato.devs.immediate-system :refer [immediate-step]]))

(defn aF-real-time-system-start!
  "sim is an instance of a network-simulator.

  start-time is the simulation starting time, in milliseconds.

  max-delta is the maximum step size the simulation will make, in
  milliseconds. The actual step size depends on
  requestAnimationFrame. max-delta prevents the simulation from
  attempting to do too much work in a single step. For example, if the
  simulation is in a tab that does not have focus, it will not update
  until the user refocuses the tab. It may be very expensive to try to
  catch up to the current wallclock time. Note that the system will
  not try to make up for lost time. It will update once, up to
  max-delta, and then resume updating at the rate dictated by
  requestAnimationFrame.

  input! is a function that takes no arguments and returns a seq of
  input values.

  output! is a function takes a [time output] pair and returns a value
  that will be ignored.

  The trailing bang indicates that these functions will most likely
  employ side-effects.

  Returns a handle that can be passed to aF-real-time-system-stop! to
  stop the system."
  ([sim start-time max-delta input!]
   (aF-real-time-system-start! sim start-time max-delta input! (constantly nil)))
  ([sim start-time max-delta input! output!]
   (let [aF (atom nil)]
     (letfn [(step [sim sim-t t t']
               (let [delta      (- t' t)
                     delta'     (min delta max-delta)
                     sim-t'     (+ sim-t delta')
                     ev*        (input!)
                     tmsg*      (map (fn [m] [(dec sim-t') m]) ev*)
                     [sim' out] (immediate-step sim (tl sim) sim-t' tmsg*)]
                 (doseq [x out] (output! x))
                 (reset! aF (js/requestAnimationFrame (fn [t] (step sim' sim-t' t' t))))))]
       (step (init sim start-time) start-time (now) (now))
       aF))))

(defn aF-real-time-system-stop! [handle]
  (js/cancelAnimationFrame @handle)
  nil)
