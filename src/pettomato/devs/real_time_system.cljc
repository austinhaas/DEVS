(ns pettomato.devs.real-time-system
  (:require
   #?(:clj  [clojure.core.async :as async :refer [timeout close! alts! go <! >! chan]]
      :cljs [cljs.core.async :as async :refer [timeout close! alts! <! >! chan]])
   [pettomato.devs.util :refer [infinity now]]
   [pettomato.devs.immediate-system :refer [immediate-step]]
   [pettomato.devs.Simulator :refer [init int-update ext-update con-update tl tn]])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

;; Note that with Clojurescript, core.async's timeout depends on
;; Javascript's setTimeout, which may not be accurate. It could cause
;; the timeout to be longer than what was requested. If there are many
;; events that are very close together, this could cause the
;; simulation to run slower than real-time.

(defn real-time-system
  [sim start-time chan-in chan-out]
  (let [wc-start-time (now)]
    (go
      (loop [sim (init sim start-time)]
        (let [sim-t  (+ start-time (- (now) wc-start-time))
              tn     (tn sim)
              delta  (- tn sim-t)
              [v ch] (cond
                       (<= delta 0)       [nil nil]
                       (= delta infinity) [(<! chan-in) chan-in]
                       :else              (alts! [chan-in (timeout delta)]))
              ;; Get sim-t again, since some time may have elapsed
              ;; waiting for the next message or event.
              sim-t  (+ start-time (- (now) wc-start-time))]
          (if (= ch chan-in)
            (if (nil? v)
              (println "Stopping real-time-system.")
              (let [tmsg*      (map (fn [m] [(dec sim-t) m]) v)
                    [sim' out] (immediate-step sim (tl sim) sim-t tmsg*)]
                (when (seq out) (>! chan-out out))
                (recur sim')))
            (let [[sim' out] (immediate-step sim (tl sim) sim-t)]
              (when (seq out) (>! chan-out out))
              (recur sim'))))))))
