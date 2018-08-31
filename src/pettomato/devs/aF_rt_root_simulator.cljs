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
  (atom {:root-sim   (rs/root-simulator sim start-time)
         :start-time start-time
         :max-delta  max-delta
         :output!    output!
         :af-handle  nil}))

;; The advance function is replaced by start! and stop!.

;; time is decremented to avoid confluence issues with messages that
;; could still arrive at the current time.

(defn step [root-sim max-delta last-sim-time last-wall-time curr-wall-time output!]
  (let [actual-delta         (- curr-wall-time last-wall-time)
        adjusted-delta       (min actual-delta max-delta)
        curr-sim-time        (+ last-sim-time adjusted-delta)
        [root-sim' tmsg-out] (rs/advance root-sim curr-sim-time)]
    (doseq [tmsg tmsg-out] (output! tmsg))
    (fn [_] (step root-sim' curr-sim-time curr-wall-time (now) output!))))

(defn start! [pkg]
  (swap! pkg (fn [{:keys [root-sim start-time max-delta] :as pkg}]
               (assoc pkg :af-handle (rAF-start! (step root-sim max-delta start-time (now) (now)))))))

(defn stop! [pkg]
  (rAF-stop! (:af-handle @pkg))
  pkg)

(defn schedule! [pkg t msg]
  (swap! pkg update :root-sim rs/schedule t msg)
  pkg)

(defn schedule*! [pkg tmsgs]
  (swap! pkg update :root-sim rs/schedule* tmsgs)
  pkg)

(defn schedule-now! [pkg msg]
  (schedule! pkg (now) msg))
