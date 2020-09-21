(ns pettomato.devs.parallel.models.examples
  (:require
   [pettomato.devs.parallel.models.atomic :refer [atomic-model]]
   [pettomato.devs.util :refer [infinity]]))

(defn generator
  "A model that periodically emits value on a port labeled :out."
  [value period]
  (atomic-model
   (let [s nil
         e 0]
     [s e])
   identity
   nil
   nil
   (constantly {:out [value]})
   (constantly period)))

(defn lazy-seq-generator
  "A model that emits values according to a (possibly lazy and infinite) seq
  of [sigma mail]."
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

(defn delay-component
  "A model takes takes a value on port :in and after processing-time, emits the
  same value on port :out.

  Ignores input while processing. If multiple inputs arrive at the same time,
  one is chosen at random."
  [processing-time]
  (atomic-model
   (let [s ["passive" infinity 1]
         e 0]
     [s e])
   (fn [s]
     (let [[phase sigma store] s]
       ["passive" infinity store]))
   (fn [s e x]
     (let [[phase sigma store] s
           v                   (rand-nth (:in x))]
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
