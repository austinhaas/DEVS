(ns pettomato.devs.parallel.models.examples
  (:require
   [pettomato.devs.parallel.models.atomic :refer [atomic-model]]
   [pettomato.devs.util :refer [infinity]]))

(defn generator [value period]
  (atomic-model
   (let [s nil
         e 0]
     [s e])
   identity
   nil
   nil
   (constantly {:out [value]})
   (constantly period)))

(defn delay-component [processing-time]
  (atomic-model
   (let [s ["passive" infinity 1]
         e 0]
     [s e])
   (fn [s]
     (let [[phase sigma store] s]
       ["passive" infinity store]))
   (fn [s e x]
     (let [[phase sigma store] s
           v                   (first (:in x))]
       (if (= phase "passive")
         ["busy" processing-time v]
         ["passive" (- sigma e) store])))
   nil
   (fn [s]
     (let [[phase sigma store] s]
       {:out [store]}))
   (fn [s]
     (let [[phase sigma store] s]
       sigma))))

(defn lazy-seq-generator
  "A (possibly lazy and infinite) seq of [sigma mail]."
  [s]
  (atomic-model
   (let [s s
         e 0]
     [s e])
   next
   nil
   nil
   (comp second first)
   (fn time-advance [s]
     (if (seq s)
       (ffirst s)
       infinity))))

#_
(defn switch [processing-time]
  (atomic-model
   [{:phase   :passive
     :sigma   infinity
     :inport  :in1
     :store   nil
     :switch? true} 0]
   (fn int-update [s]
     (assoc s :phase :passive :sigma infinity))
   (fn ext-update [s e x]
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
                      :in1 [[:out1 (:store s)]]
                      :in2 [[:out2 (:store s)]])
               false (case (:inport s)
                       :in1 [[:out2 (:store s)]]
                       :in2 [[:out1 (:store s)]]))))
   :sigma))
