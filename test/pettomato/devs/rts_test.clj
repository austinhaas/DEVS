(ns pettomato.devs.rts-test
  (:require
   [clojure.test :refer :all]
   [clojure.core.async :as async :refer [go chan <! timeout close! >! put!]]
   [pettomato.devs.util :refer [dissoc-in infinity now]]
   [pettomato.devs.test-util :refer [eq?]]
   [pettomato.devs.models :refer [atomic-model executive-model network-model register unregister connect disconnect]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.network-simulator :refer [network-simulator]]
   [pettomato.devs.root-simulator :as rs]))

(defn generator [value period]
  (atomic-model
   {}
   (fn [s]
     (Thread/sleep 100)
     s)
   nil
   nil
   (constantly [[:out value]])
   (constantly period)))

#_
(let [chan-in  (chan 100)
      chan-out (chan 100)
      sim      (atomic-simulator (generator 5 10))]

  (go (loop [i 0]
        (when (> i 10)
          (close! chan-in))
        (if-let [v (<! chan-out)]
          (do (println "[" (first v) "]" (second v))
              (recur (inc i)))
          (println 'done))))

  (real-time-system sim 0 chan-in chan-out))

(defn switch [processing-time]
  (atomic-model
   {:phase   :passive
    :sigma   infinity
    :inport  :in1
    :store   nil
    :switch? true}
   (fn int-update [s]
     (assoc s :phase :passive :sigma infinity))
   (fn ext-update [s e x]
     (assert (= 1 (count x)))
     (let [[port val] (first x)]
       (if (= (:phase s) :passive)
         (assoc s
                :phase  :busy
                :sigma   processing-time
                :inport  port
                :store   val
                :switch? (not (:switch? s)))
         (assoc s :sigma (- (:sigma s) e)))))
   nil
   (fn output [s]
     (case (:phase s)
       :busy (case (:switch? s)
               true (case (:inport s)
                      :in1 [[:out1  (:store s)]]
                      :in2 [[:out2  (:store s)]])
               false (case (:inport s)
                       :in1 [[:out2  (:store s)]]
                       :in2 [[:out1  (:store s)]]))))
   :sigma))

#_
(let [chan-in  (chan 100)
      chan-out (chan 100)
      sim      (atomic-simulator (switch 100))
      input    [[100 [[:in1 1]]]
                [150 [[:in1 2]]]
                [200 [[:in1 3]]]
                [300 [[:in1 4]]]
                [400 [[:in2 5]]]]
      wc-start (now)]

  (go (loop []
        (if-let [v (<! chan-out)]
          (do (println "[" (first v) "]" (second v))
              (recur))
          (println 'done))))

  (real-time-system sim 0 chan-in chan-out)

  (go (loop [input input]
        (when (seq input)
          (let [[t msg] (first input)
                t'      (+ wc-start t)
                wc-t    (now)
                dt      (- t' wc-t)]
            (<! (timeout dt))
            (>! chan-in msg)
            (recur (rest input))))))

  (go (<! (timeout 2000))
      (close! chan-in)
      (close! chan-out)))
