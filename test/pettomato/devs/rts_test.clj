(ns pettomato.devs.rts-test
  (:require
   [clojure.test :refer :all]
   [clojure.core.async :as async :refer [go chan <! timeout close! >! put! onto-chan]]
   [pettomato.devs.util :refer [dissoc-in infinity now]]
   [pettomato.devs.test-util :refer [eq?]]
   [pettomato.devs.models :refer [atomic-model executive-model network-model register unregister connect disconnect]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.network-simulator :refer [network-simulator]]
   [pettomato.devs.real-time-system :refer [real-time-system]]
   [pettomato.devs.root-simulator :as rs]))

(defn generator [value period]
  (atomic-model
   {}
   (fn [s]
     ;; Uncomment this sleep expression to see what happens when the
     ;; simulator can't catch up.
     ;;(Thread/sleep 15)
     s)
   nil
   nil
   (constantly [[:out value]])
   (constantly period)))

(comment

  (let [chan-in  (chan 100)
        chan-out (chan 100)
        sim      (atomic-simulator (generator 5 1000))]

    ;; Print 10 values.
    (go (loop [i 0]
          (if (> i 5)
            (close! chan-in)
            (if-let [v (<! chan-out)]
              (do (printf "v: %s\n" v)
                  (recur (inc i)))
              (println 'done)))))

    (real-time-system sim 0 chan-in chan-out true))

  )

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
     (assert (= 1 (count x)) x)
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

(comment

 (let [chan-in  (chan 100)
       chan-out (chan 100)
       sim      (atomic-simulator (switch 100))
       input    [[100 [:in1 1]]
                 [150 [:in1 2]]
                 [200 [:in1 3]]
                 [300 [:in1 4]]
                 [400 [:in2 5]]]]

   (go (loop []
         (if-let [v (<! chan-out)]
           (do (printf "v: %s\n" v)
               (recur))
           (println 'done))))

   (onto-chan chan-in input false)

   (real-time-system sim 0 chan-in chan-out true)

   #_
   (let [wc-start (now)]
     (go (loop [input input]
           (when (seq input)
             (let [[t msg] (first input)
                   t'      (+ wc-start t)
                   wc-t    (now)
                   dt      (- t' wc-t)]
               (<! (timeout dt))
               (>! chan-in msg)
               (recur (rest input)))))))

   (go (<! (timeout 2000))
       (close! chan-in)
       (close! chan-out)))

 )
