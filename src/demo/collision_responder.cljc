(ns demo.collision-responder
  (:require
   [pt-lib.geometry.2D.rect :as rect]
   [pt-lib.geometry.2D.point :as pt]
   [pt-lib.match :refer [match]]
   [pt-lib.math :refer [abs]]
   [pt-lib.number :refer [infinity]]
   [devs.models :refer [atomic-model]]))

;; http://www.gamasutra.com/view/feature/131790/simple_intersection_tests_for_games.php?page=3

(defn AABB-AABB-separation-vector [a0 a1 b0 b1]
  ;; Returns a vector that can be applied to a1 to separate it from
  ;; b1.
  (let [p (rect/nearest-pt-in-rect-to-pt a0 (rect/center b0))
        q (rect/nearest-pt-in-rect-to-pt b0 p)
        [xa ya wa ha] a1
        [xb yb wb hb] b1
        a-x [(- xb (+ xa wa)) 0]
        a+x [(- (+ xb wb) xa) 0]
        a-y [0 (- yb (+ ya ha))]
        a+y [0 (- (+ yb hb) ya)]]
    (if (= p q)
      ;; Objects were already overlapping during the previous
      ;; frame. Ignore previous positions; push the objects apart
      ;; across the shortest overlap.
      (min-key (fn length-squared [[x y]] (+ (* x x) (* y y))) a-x a+x a-y a+y)
      ;; contact-normal
      (let [[nx ny] (pt/sub p q)]
        (if (> (abs nx) (abs ny))
          (if (neg? nx) a-x a+x)
          (if (neg? ny) a-y a+y))))))

(defn collision-responder []
  (atomic-model
   {:vel    {}
    :output []
    :sigma  infinity}
   (fn int-update [s] (assoc s :output [] :sigma infinity))
   (fn ext-update [s e x]
     (let [[vel out]
           (reduce (fn [[vel out] ev]
                     (match ev
                       [:add [k p v e]]  [(assoc vel k v) out]
                       [:vel [k v]]      [(assoc vel k v) out]
                       [:coll-start val] (let [[[a [ax ea]] [b [bx eb]]] (seq val)
                                               va    (vel a)
                                               vb    (vel b)]
                                           [vel (conj out
                                                      [:vel [a (* -1 va)]]
                                                      [:vel [b (* -1 vb)]])])))
                   [(:vel s) []]
                   x)]
       (assoc s :vel vel :output out :sigma 0)))
   nil
   :output
   :sigma))
