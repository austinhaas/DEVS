(ns demo.integrator
  (:require
   [pt-lib.coll :refer [group]]
   [pt-lib.physics.integration :refer [euler-step]]
   [devs.atomic-model :refer [atomic-model]]))

(defn integrator [k pos vel h offset]
  (atomic-model
   (let [sigma (mod (- h offset) h)]
     {:pos   pos
      :pos2  (euler-step vel pos sigma)
      :vel   vel
      :sigma sigma})
   (fn int-update [s]
     (assoc s
            :pos   (:pos2 s)
            :pos2  (euler-step (:vel s) (:pos2 s) h)
            :sigma h))
   (fn ext-update [s e x]
     (let [pos       (:pos s)
           vel       (:vel s)
           sigma'    (- (:sigma s) e)
           port->ev* (group first second [] x)
           pos'      (last (port->ev* :pos))
           vel'      (or (last (port->ev* :vel)) vel)
           pos1      (euler-step vel pos e)
           pos2      (euler-step vel' (or pos' pos1) sigma')]
       (assoc s
              :pos   pos1
              :pos2  pos2
              :vel   vel'
              :sigma sigma')))
   nil
   (fn output [s] [[:pos [k (:pos2 s)]]])
   :sigma))
