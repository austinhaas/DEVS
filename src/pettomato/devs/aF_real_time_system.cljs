(ns pettomato.devs.aF-real-time-system
  (:require
   [pettomato.devs.util :refer [now]]
   [pettomato.devs.Simulator :refer [init tl]]
   [pettomato.devs.simulation-advance :refer [advance]]))

(defn rAF-start
  "Repeatedly calls a function f according to requestAnimationFrame,
  presumably for side-effects. f returns either a new update function,
  to replace f in the next call, or a false value indicating that the
  update cycle should end immediately.

  Note that the time passed to f is the current time, not the time
  that requestAnimationFrame was invoked (which is what callbacks to
  rAF usually receive).

  https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame"
  [f]
  (let [handle (atom nil)]
    (letfn [(tick []
              (when-let [tick (f (now))]
                (reset! handle (js/requestAnimationFrame tick))))]
      (reset! handle (js/requestAnimationFrame tick)))
    handle))

(defn rAF-stop
  [handle]
  (js/cancelAnimationFrame @handle))

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

  https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame

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
   (letfn [(tick [sim last-sim-time last-wall-time curr-wall-time]
             (let [actual-delta    (- cur-wall-time last-wall-time)
                   adjusted-delta  (min actual-delta max-delta)
                   curr-sim-time   (+ last-sim-time adjusted-delta)
                   tmsg-in         (input!)
                   [sim' tmsg-out] (advance sim curr-sim-time tmsg-in)]
               (doseq [tmsg tmsg-out] (output! tmsg))
               (fn [_] (tick sim' curr-sim-time curr-wall-time (now)))))]
     (rAF-start (tick (init sim start-time) start-time (now) (now))))))

;; Compose a function to calculate the current sim time.
;; (May need to be stateful.)

;; (defn governor [max-delta initial-value]
;;   (let [prev (atom initial-value)]
;;     (fn [curr]
;;       ;; This isn't what we want. We want to reset the target time so
;;       ;; that we don't have to keep trying to catch up.

;;       (let [delta (min (- curr @prev))])
;;       )))

;; (comp (partial max max-delta) now)


(defn aF-real-time-system-stop! [handle]
  (rAF-stop handle))
