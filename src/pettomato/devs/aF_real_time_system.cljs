(ns pettomato.devs.aF-real-time-system
  (:require
   [pettomato.devs.util :refer [now get-and-set!]]
   [pettomato.devs.Simulator :refer [init tl]]
   [pettomato.devs.immediate-system :refer [immediate-step]]))

(defn aF-real-time-system-start!
  ([sim start-time input] (aF-real-time-system-start! sim start-time input nil))
  ([sim start-time input output]
   (let [aF (atom nil)
         wc-start-time (now)]
     (letfn [(step [sim t]
               (let [sim-t (+ start-time (- t wc-start-time))
                     ev*   (get-and-set! input [])
                     tmsg* (map (fn [m] [(dec sim-t) m]) ev*)
                     [sim' out] (immediate-step sim (tl sim) sim-t tmsg*)]
                 (when (and output (seq out)) (swap! output conj [sim-t out]))
                 (reset! aF (js/requestAnimationFrame (fn [t] (step sim' t))))))]
       (step (init sim start-time) (now))
       aF))))

(defn aF-real-time-system-stop! [handle]
  (js/cancelAnimationFrame @handle)
  nil)
