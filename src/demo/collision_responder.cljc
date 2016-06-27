(ns demo.collision-responder
  (:require
   [pt-lib.match :refer [match]]
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

(defn- normalize-event [[port val]]
  (if (vector? port)
    (let [[port' k] port]
      [port' [k val]])
    [port val]))

(defn- port-sym [ev]
  (let [[port val] ev]
    (if (vector? port)
      (first port)
      port)))

(defn collision-responder2 []
  (atomic-model
   {:vel    {}
    :output []
    :sigma  infinity}
   (fn int-update [s] (assoc s :output [] :sigma infinity))
   (fn ext-update [s e x]
     (let [[vel out]
           (reduce (fn [[vel out] ev]
                     (match ev
                       [[:add k] [p v e]] [(assoc vel k v) out]
                       [[:vel k] v]       [(assoc vel k v) out]
                       [:coll-start val]  (let [[a b] (vec val)
                                                va    (get-in s [:vel a])
                                                vb    (get-in s [:vel b])]
                                            [vel (conj out
                                                       [[:vel a] (* -1 va)]
                                                       [[:vel b] (* -1 vb)])])))
                   [(:vel s) []]
                   x)]
       (assoc s :vel vel :output out :sigma 0)))
   nil
   :output
   :sigma))
