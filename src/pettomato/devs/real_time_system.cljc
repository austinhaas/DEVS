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
#_
(defn real-time-system [sim start-time chan-in chan-out]
  (let [sim      (init sim start-time)
        wc-start (now)]
    (go
      (loop [sim   sim
             wc-tl wc-start]
        (if (= (tn sim) (tl sim)) ;; Optimization: ignore input until time advances.
          (let [[sim' out] (int-update sim (tn sim))]
            (recur sim' wc-tl))
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
              (let [wc-t   (now)
                    wc-e   (- wc-t wc-tl)
                    sim-t  (min (+ sim-tl wc-e) sim-tn)
                    msg*   (if v [v] [])
                    int-tn (tn sim)
                    ext-tn (if (seq msg*) sim-t infinity)]
                (cond
                  (< int-tn ext-tn) (let [[sim' out] (int-update sim sim-t)]
                                      (doseq [msg out] (>! chan-out [[(- wc-t wc-start) sim-t] msg]))
                                      (recur sim' wc-t))
                  (< ext-tn int-tn) (let [sim' (ext-update sim msg* sim-t)]
                                      (recur sim' wc-t))
                  :else             (let [[sim' out] (con-update sim msg* sim-t)]
                                      (doseq [msg out] (>! chan-out [[(- wc-t wc-start) sim-t] msg]))
                                      (recur sim' wc-t))))))))))
  true)

(defn real-time-system [sim start-time chan-in chan-out]
  (let [sim      (init sim start-time)
        wc-start (now)]
    (go
      (loop [sim   sim
             wc-tl wc-start]
        (let [wc-tn  (+ wc-tl (- (tn sim) (tl sim)))
              dt     (- wc-tn (now))]
          (let [tout   (timeout-inf dt)
                [v ch] (alts! [chan-in tout] :priority true)
                wc-t   (now)
                wc-e   (- wc-t wc-start)]
            (condp = ch
              tout    (let [[sim' out] (int-update sim (tn sim))]
                        (doseq [msg out] (>! chan-out [[wc-e (tn sim)] msg]))
                        (recur sim' wc-t))
              chan-in (cond
                        (nil? v)       (close! chan-out)
                        (< wc-t wc-tn) (let [sim-t (min (+ (tl sim) (- wc-t wc-tl)) (tn sim))
                                             sim'  (ext-update sim [v] sim-t)]
                                         (recur sim' wc-t))
                        :else          (let [[sim' out] (con-update sim [v] (tn sim))]
                                         (doseq [msg out] (>! chan-out [[wc-e (tn sim)] msg]))
                                         (recur sim' wc-t)))))))))
  true)
