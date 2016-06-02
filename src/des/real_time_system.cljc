(ns des.real-time-system
  (:require
   [clojure.core.async :as async :refer [timeout close! alts! go >! chan]]
   [pt-lib.number :refer [infinity]]
   [pt-lib.date :refer [now]]
   [des.Simulator :refer [init int-update ext-update tl tn]]))

(defn real-time-system [sim start-time chan-in chan-out]
  (go
    (loop [sim   (init sim start-time)
           wc-tl (now)]
      (let [wc-t   (now)
            tout   (if (< (tn sim) infinity)
                     (let [wc-e (- wc-t wc-tl)
                           t    (min (+ (tl sim) wc-e) (tn sim))]
                       (timeout (- (tn sim) t)))
                     (chan 1))
            [v ch] (alts! [tout chan-in])]
        (condp = ch
          chan-in (if (nil? v)
                    (do (println 'done) (close! chan-out))
                    (let [wc-t (now)
                          wc-e (- wc-t wc-tl)
                          t    (min (+ (tl sim) wc-e) (tn sim))
                          sim' (ext-update sim v t)]
                      (recur sim' wc-t)))
          tout    (let [[sim' ev*] (int-update sim (tn sim))]
                    (doseq [ev ev*] (>! chan-out [(tn sim) ev]))
                    (recur sim' wc-t))))))
  true)
