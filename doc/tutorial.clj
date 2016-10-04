(ns tutorial
  (:require
   [clojure.core.async :as async :refer [<! chan close! go put!]]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.immediate-system :refer [immediate-system]]
   [pettomato.devs.real-time-system :refer [real-time-system]]
   [pettomato.devs.models :refer [atomic-model]]))

;;; A simple atomic model.

;; The model is initially passive. Upon receiving a message,

(defn timer [duration]
  (atomic-model
   {:phase 'passive
    :sigma infinity}
   (fn int-update [s]
     {:phase 'passive :sigma infinity})
   (fn ext-update [s e x]
     (case (:phase s)
       passive {:phase 'active  :sigma duration}
       active  {:phase 'passive :sigma infinity}))
   nil
   (fn output [s] "Ding!")
   :sigma))


(-> (timer 3000)
    atomic-simulator
    (immediate-system 0 10000 [[1000 nil]]))

;; The simulation can be run in real-time using the real-time
;; simulator.

(defn listen! [prompt ch]
  (println "Listening...")
  (go (loop []
        (if-let [v (<! ch)]
          (do (println prompt v)
              (recur))
          (println "Listening done.")))))

(def chan-in  (chan 10))
(def chan-out (chan 10))

(listen! "> " chan-out)

(-> (timer 3000)
    atomic-simulator
    (real-time-system 0 chan-in chan-out))

(put! chan-in [nil])

(close! chan-in)
(close! chan-out)
