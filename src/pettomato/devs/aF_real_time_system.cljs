(ns pettomato.devs.aF-real-time-system
  (:require
   [pettomato.devs.util :refer [now get-and-set!]]
   [pettomato.devs.Simulator :refer [init tl]]
   [pettomato.devs.immediate-system :refer [immediate-step]]))

(defn aF-real-time-system-start!
  ([sim start-time max-delta input]
   (aF-real-time-system-start! sim start-time max-delta input nil))
  ([sim start-time max-delta input output]
   (let [aF (atom nil)]
     (letfn [(step [sim sim-t t t']
               (let [delta      (- t' t)
                     delta'     (min delta max-delta)
                     sim-t'     (+ sim-t delta')
                     ev*        (get-and-set! input [])
                     tmsg*      (map (fn [m] [(dec sim-t') m]) ev*)
                     [sim' out] (immediate-step sim (tl sim) sim-t' tmsg*)]
                 (when (and output (seq out)) (swap! output conj [sim-t' out]))
                 (reset! aF (js/requestAnimationFrame (fn [t] (step sim' sim-t' t' t))))))]
       (step (init sim start-time) start-time (now) (now))
       aF))))

(defn aF-real-time-system-stop! [handle]
  (js/cancelAnimationFrame @handle)
  nil)
