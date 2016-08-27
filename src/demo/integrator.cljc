(ns pettomato.demo.integrator
  (:require
   [pettomato.lib.match :refer [match]]
   [pettomato.lib.coll :refer [group]]
   [pettomato.lib.physics.integration :refer [first-order-euler]]
   [pettomato.devs.models :refer [atomic-model]]))

(defn integrator [k pos vel h offset]
  (atomic-model
   (let [sigma (mod (- h offset) h)]
     {:pos   pos
      :pos2  (first-order-euler vel pos sigma)
      :vel   vel
      :sigma sigma})
   (fn int-update [s]
     (assoc s
            :pos   (:pos2 s)
            :pos2  (first-order-euler (:vel s) (:pos2 s) h)
            :sigma h))
   (fn ext-update [s e x]
     (let [pos       (:pos s)
           vel       (:vel s)
           sigma'    (- (:sigma s) e)
           port->ev* (group first second [] x)
           pos'      (last (port->ev* :pos))
           vel'      (or (last (port->ev* :vel)) vel)
           pos1      (first-order-euler vel pos e)
           pos2      (first-order-euler vel' (or pos' pos1) sigma')]
       (assoc s
              :pos   pos1
              :pos2  pos2
              :vel   vel'
              :sigma sigma')))
   nil
   (fn output [s] [[:pos [k (:pos2 s)]]])
   :sigma))

(defn mult-integrator [h]
  (atomic-model
   {:pos1   {}
    :pos2   {}
    :vel    {}
    :sigma  h
    :output []}
   (fn int-update [s]
     (if (seq (:output s))
       (assoc s :output [])
       (let [vel  (:vel s)
             pos1 (:pos2 s)
             pos2 (reduce-kv (fn [m k p]
                               (assoc m k (first-order-euler (vel k) p h)))
                             {}
                             pos1)]
         (assoc s
                :pos1  pos1
                :pos2  pos2
                :output (for [[k p] pos1] [:pos [k p]])
                :sigma h))))
   (fn ext-update [s e x]
     (let [sigma (- (:sigma s) e)
           [pos1 pos2 vel out]
           (reduce (fn [[pos1 pos2 vel out] ev]
                     (match ev
                       [:add [k p v]] [(assoc pos1 k p)
                                       (assoc pos2 k (first-order-euler v p sigma))
                                       (assoc vel  k v)
                                       (if (= sigma h)
                                         (conj out [:pos [k p]])
                                         out)]
                       [:rem k]       [(dissoc pos1 k) (dissoc pos2 k) (dissoc vel k) out]
                       [:vel [k v]]   (let [v' ((:vel  s) k)
                                            p  ((:pos1 s) k)
                                            p1 (first-order-euler v' p e)]
                                        [(assoc pos1 k p1)
                                         (assoc pos2 k (first-order-euler v p1 sigma))
                                         (assoc vel  k v)
                                         (if (and (= sigma h) (not= p1 p))
                                           (conj out [:pos [k p1]])
                                           out)])))
                   [(:pos1 s) (:pos2 s) (:vel s) []]
                   x)]
       (assoc s
              :pos1   pos1
              :pos2   pos2
              :vel    vel
              :output out
              :sigma  sigma)))
   nil
   :output
   (fn time-advance [s]
     (if (seq (:output s))
       0
       (:sigma s)))))
