(ns pettomato.devs.aF-rt-root-simulator
  "A stepwise, real-time root-simulator that is based on
  requestAnimationFrame."
  (:require
   [pettomato.devs.util :refer [now]]
   [pettomato.devs.Simulator :refer [init tl]]
   [pettomato.devs.root-simulator :as rs]))

(defn- rAF-start!
  "Repeatedly calls a function f according to requestAnimationFrame,
  presumably for side-effects. f returns either a new function, to
  replace f in the next call, or a false value indicating that the
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

(defn- rAF-stop!
  [handle]
  (js/cancelAnimationFrame @handle))

;;------------------------------------------------------------------------------

(defn aF-rt-root-simulator [sim start-time max-delta output!]
  (let [af-handle nil]
    (atom [(rs/root-simulator sim start-time) max-delta output! af-handle])))

;; The advance function is replaced by start! and stop!.

;; time is decremented to avoid confluence issues with messages that
;; could still arrive at the current time.

(defn step [root last-sim-time last-wall-time curr-wall-time output!]
  (let [actual-delta     (- cur-wall-time last-wall-time)
        adjusted-delta   (min actual-delta max-delta)
        curr-sim-time    (+ last-sim-time adjusted-delta)
        [root' tmsg-out] (rs/advance root curr-sim-time)]
    (doseq [tmsg tmsg-out] (output! tmsg))
    (fn [_] (step root' curr-sim-time curr-wall-time (now) output!))))

(defn start! [rootx]
  (let [[root max-delta output! _] @rootx]
    ;; This root is probably wrong.
    [reset! rootx [root max-delta output (rAF-start! (tick root start-time (now) (now)))]]))

(defn stop! [root]
  (let [[_ _ _ af-handle] @root]
    (rAF-stop! af-handle)
    root))

(defn schedule! [root t msg]
  (swap! root update 0 rs/schedule t msg)
  root)

(defn schedule*! [root tmsgs]
  (swap! root update 0 rs/schedule* tmsgs)
  root)

(defn schedule-now! [root msg]
  (rs/schedule! root (now) msg))
