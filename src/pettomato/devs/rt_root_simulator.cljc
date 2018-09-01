(ns pettomato.devs.rt-root-simulator
  (:require
   #?(:clj  [clojure.core.async :as async :refer [timeout close! alts! go <! >! chan]]
      :cljs [cljs.core.async :as async :refer [timeout close! alts! <! >! chan]])
   [pettomato.devs.util :refer [infinity now]]
   [pettomato.devs.root-simulator-base :as rs])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

;; Note that with Clojurescript, core.async's timeout depends on
;; Javascript's setTimeout, which may not be accurate. It could cause
;; the timeout to be longer than what was requested. If there are many
;; events that are very close together, this could cause the
;; simulation to run slower than real-time.

(defn rt-root-simulator
  "Runs until chan-in is closed."
  [sim sim-start-time chan-in chan-out close?]
  (let [wc-start-time (now)]
    (go
      (loop [root-sim           (rs/root-simulator sim sim-start-time)
             time-of-next-event (rs/time-of-next-event root-sim)]
        (let [wc-delta  (- (now) wc-start-time)
              sim-t     (+ sim-start-time wc-delta)
              ;; TODO: Handle confluence.
              [tmsg ch] (cond
                          (<= time-of-next-event sim-t)   [nil nil]
                          (= time-of-next-event infinity) [(<! chan-in) chan-in]
                          :else                           (alts! [chan-in (timeout (- time-of-next-event sim-t))]))
              ;; Get sim-t again, since some time may have elapsed
              ;; waiting for the next message or event.
              wc-delta  (- (now) wc-start-time)
              sim-t     (+ sim-start-time wc-delta)]
          (if (= ch chan-in)
            (if (nil? tmsg)
              (when close? (close! chan-out))
              (let [root-sim       (let [[t msg] tmsg] (rs/schedule root-sim t msg))
                    [root-sim out] (rs/advance root-sim sim-t)]
                (when (seq out) (>! chan-out out))
                (recur root-sim (rs/time-of-next-event root-sim))))
            (let [[root-sim out] (rs/advance root-sim sim-t)]
              ;;(printf "out: %s\n" out)
              (when (seq out) (>! chan-out out))
              (recur root-sim (rs/time-of-next-event root-sim)))))))))
