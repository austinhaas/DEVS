(ns pettomato.devs.real-time-system
  (:require
   #?(:clj  [clojure.core.async :as async :refer [timeout close! alts! go <! >! chan]]
      :cljs [cljs.core.async :as async :refer [timeout close! alts! <! >! chan]])
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.util :refer [now]]
   [pettomato.devs.Simulator :refer [init int-update ext-update tl tn]])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(defn timeout-inf [msecs]
  (if (< msecs infinity)
    (timeout msecs)
    (chan 1)))

;; This version works, but it prioritizes internal updates. Ideally,
;; that would be left to the simulator to decide, using a confluence
;; function. The only solution I can imagine is to wait until the
;; clock advances to time t+1 before processing events at time t.

;; Need to produce a log of incoming events with sim timestamps in
;; order to reproduce a run.
#_
(defn real-time-system [sim start-time chan-in chan-out]
  (let [wc-start (now)]
    (go
      (loop [sim   (init sim start-time)
             wc-tl (now)]
        (let [wc-t   (now)
              wc-e   (- wc-t wc-tl)
              sim-tl (tl sim)
              sim-tn (tn sim)
              sim-t  (+ sim-tl wc-e)]
          (cond
            (> sim-t sim-tn) (do (println (format "sim is behind by %s msecs." (- sim-t sim-tn)))
                                 (let [sim-t      sim-tn
                                       [sim' ev*] (int-update sim sim-t)]
                                   (doseq [ev ev*] (>! chan-out [[(- wc-t wc-start) sim-t] ev]))
                                   (recur sim' wc-t)))
            (= sim-t sim-tn) (let [[sim' ev*] (int-update sim sim-t)]
                               (doseq [ev ev*] (>! chan-out [[(- wc-t wc-start) sim-t] ev]))
                               (recur sim' wc-t))
            (< sim-t sim-tn) (let [sim-dt (- sim-tn sim-t)
                                   tout   (timeout-inf sim-dt)
                                   [v ch] (alts! [tout chan-in] :priority true)]
                               (condp = ch
                                 chan-in (if (nil? v)
                                           (close! chan-out)
                                           (let [wc-t   (now)
                                                 wc-e   (- wc-t wc-tl)
                                                 sim-tl (tl sim)
                                                 sim-tn (tn sim)
                                                 sim-t  (+ sim-tl wc-e)
                                                 sim' (ext-update sim v sim-t)]
                                             (recur sim' wc-t)))
                                 tout    (recur sim wc-tl))))))))
  true)

;; This is a placeholder.
(defn update-sim [sim t msg*]
  (loop [sim  sim
         msg* msg*
         out  []]
    (cond
      (= (tn sim) t) (let [[sim' msg*'] (int-update sim t)]
                       (recur sim' msg* (into out msg*')))
      (seq msg*)     (recur (ext-update sim msg* t) [] out)
      :else          [sim out])))

;; I believe this version correctly sequences internal and external
;; events. The key idea is that before the next event is processed,
;; external events are collected until the clock advances, preventing
;; any straggler events from occurring at the same time. The
;; additional latency seems negligible.
(defn real-time-system [sim start-time chan-in chan-out]
  (let [sim      (init sim start-time)
        wc-start (now)]
    (go
      (loop [sim   sim
             wc-tl (now)]
        (let [wc-t   (now)
              wc-e   (- wc-t wc-tl)
              sim-tl (tl sim)
              sim-tn (tn sim)
              sim-t  (+ sim-tl wc-e)
              sim-dt (- sim-tn sim-t)
              tout   (timeout-inf sim-dt)
              [v ch] (alts! [chan-in tout] :priority true)]
          (if (and (= ch chan-in) (nil? v))
            (close! chan-out)
            (let [wc-t  (now)
                  wc-e  (- wc-t wc-tl)
                  sim-t (min (+ sim-tl wc-e) sim-tn)
                  msg*  (loop [acc (if v [v] [])]
                          (if (> (now) wc-t)
                            acc
                            (let [[v ch] (alts! [chan-in (timeout 1)] :priority true)]
                              (if (nil? v)
                                (if (= ch chan-in)
                                  acc
                                  (recur acc))
                                (recur (conj acc v))))))
                  [sim' msg*'] (update-sim sim sim-t msg*)]
              (doseq [msg msg*'] (>! chan-out [[(- wc-t wc-start) sim-t] msg]))
              (recur sim' wc-t)))))))
  true)
