(ns pettomato.devs.real-time-system
  (:require
   #?(:clj  [clojure.core.async :as async :refer [timeout close! alts! go <! >! chan]]
      :cljs [cljs.core.async :as async :refer [timeout close! alts! <! >! chan]])
   [pettomato.devs.util :refer [infinity now]]
   [pettomato.devs.Simulator :refer [init int-update ext-update con-update tl tn]])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(defn timeout-inf [msecs]
  (if (< msecs infinity)
    (timeout msecs)
    (chan 1)))

;; I don't think this is a good implementation. With clojurescript and
;; fine grained events (events that are very close together in time),
;; this method will drift badly, causing the simulation to run much
;; slower than real-time. This is due to the inaccuracy of setTimeout
;; in browsers.

(defn real-time-system [sim start-time chan-in chan-out]
  (let [sim      (init sim start-time)
        wc-start (now)]
    (go
      (loop [sim   sim
             wc-tl wc-start]
        (let [wc-tn  (+ wc-tl (- (tn sim) (tl sim)))
              dt     (- wc-tn (now))]
          (let [tout   (timeout-inf dt)
                [v ch] (if (> dt 0)
                         (alts! [tout chan-in])
                         [nil tout])
                wc-t   (now)
                wc-e   (- wc-t wc-start)]
            (condp = ch
              tout    (let [[sim' out] (int-update sim (tn sim))]
                        (when (seq out)
                          (>! chan-out [[wc-e (tn sim)] out]))
                        (recur sim' wc-t))
              chan-in (cond
                        (nil? v)       (close! chan-out)
                        (< wc-t wc-tn) (let [sim-t (min (+ (tl sim) (- wc-t wc-tl)) (tn sim))
                                             sim'  (ext-update sim v sim-t)]
                                         (recur sim' wc-t))
                        :else          (let [[sim' out] (con-update sim v (tn sim))]
                                         (when (seq out)
                                           (>! chan-out [[wc-e (tn sim)] out]))
                                         (recur sim' wc-t)))))))))
  true)
