(ns demo.collision-responder
  (:require
   [pt-lib.coll :refer [group]]
   [pt-lib.number :refer [infinity]]
   [devs.atomic-model :refer [atomic-model]]))

(defn collision-responder []
  (atomic-model
   {:vel    {}
    :output []
    :sigma  infinity}
   (fn int-update [s] (assoc s :output [] :sigma infinity))
   (fn ext-update [s e x]
     (let [port->ev* (group first second [] x)
           vel       (:vel s)
           vel'      (into vel (port->ev* :vel))
           output    (for [ab (port->ev* :coll-start), i (vec ab)]
                       (if-let [v (vel' i)]
                         [:vel [i (* -1 v)]]
                         (assert false (format "No vel for %s." i))))]
       (assoc s :vel vel' :output output :sigma 0)))
   nil
   :output
   :sigma))
