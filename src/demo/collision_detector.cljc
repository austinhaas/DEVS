(ns demo.collision-detector
  (:require
   [clojure.set :refer [difference]]
   [pt-lib.coll :refer [group]]
   [pt-lib.number :refer [infinity]]
   [pt-lib.geometry.2D.rect :as r]
   [pt-lib.geometry.2D.point :as pt]
   [pt-lib.collision.sweep-list-2d-cache :as sl]
   [devs.models :refer [atomic-model]]))

;;; Collision priority-queue.

;; Like any priority queue, but opposite collision events cancel out.

(defn- pq-init [] ())

(defn- pq-insert [pq rec]
  (let [[t [type [a b]]] rec
        pq' (->> pq
                 (remove (fn [[t' [type' [a' b']]]]
                           (and (= #{a b} #{a' b'})
                                (not= type type')))))
        pq'' (if (= pq pq')
               (cons rec pq')
               pq')]
    (sort-by first pq'')))

(defn- pq-insert* [pq rec*] (reduce pq-insert pq rec*))

(defn- pq-peek [pq] (first pq))

(defn- pq-pop [pq] (rest pq))

(defn- pq-empty? [pq] (empty? pq))

;;--------------------------------------------------------------------------------

(defn- compute-vel [r0 r1]
  (let [[x0 y0 w0 h0] r0
        [x1 y1 w1 h1] r1
        v (pt/sub [x1 y1] [x0 y0])]
    v))

(defn- intersect-lb [a0 a1 b0 b1]
  (let [av (compute-vel a0 a1)
        bv (compute-vel b0 b1)]
    (r/intersect-lb a0 b0 av bv)))

(defn- intersect-ub [a0 a1 b0 b1]
  (let [av (compute-vel a0 a1)
        bv (compute-vel b0 b1)]
    (r/intersect-ub a0 b0 av bv)))

;;--------------------------------------------------------------------------------

(defn process-object-updates [s m step-size t]
  (let [{:keys [sl1 sl2 pq]} s
        sl2' (-> sl2
                 (sl/rem-rect*    (:delete m))
                 (sl/update-rect* (:modify m))
                 (sl/add-rect*    (:insert m)))
        sli  (sl/intersections sl2)
        sli' (sl/intersections sl2')
        add  (difference sli' sli)
        rem  (difference sli  sli')
        add' (for [[a b] (map seq add)]
               (let [ar1 (sl/lookup sl1 a)
                     ar2 (sl/lookup sl2' a)
                     br1 (sl/lookup sl1 b)
                     br2 (sl/lookup sl2' b)
                     t'  (cond
                           (nil? ar1) step-size
                           (nil? br1) step-size
                           :else      (-> (intersect-lb ar1 ar2 br1 br2)
                                          (* step-size)
                                          (min t)))]
                 [t' [:coll-start [(first a) (first b)]]]))
        rem' (for [[a b] (map seq rem)]
               (let [ar1 (sl/lookup sl1 a)
                     ar2 (sl/lookup sl2' a)
                     br1 (sl/lookup sl1 b)
                     br2 (sl/lookup sl2' b)
                     t'  (cond
                           (nil? ar2) step-size
                           (nil? br2) step-size
                           :else      (-> (intersect-ub ar1 ar2 br1 br2)
                                          (* step-size)
                                          (min t)))]
                 [t' [:coll-end [(first a) (first b)]]]))
        ev*  (->> (concat add' rem')
                  (remove (fn [[t _]] (= t infinity)))
                  ;; Aggregate events occurring at the same time.
                  (reduce (fn [acc [t ev]]
                            (if (empty? acc)
                              (conj acc [t [ev]])
                              (let [[t' ev*] (peek acc)]
                                (if (= t t')
                                  (conj (pop acc) [t' (conj ev* ev)])
                                  (conj acc [t [ev]])))))
                          []))
        pq'  (pq-insert* pq ev*)]
    (assoc s
           :sl2 sl2'
           :pq  pq')))

(defn collision-detector [step-size allow-collision?]
  (atomic-model
   (let [sl (sl/sweep-list-2d allow-collision?)]
     {:sl1 sl
      :sl2 sl
      :pq  (pq-init)
      :t   0})
   (fn int-update [s]
     (let [[t ev] (pq-peek (:pq s))]
       (-> s
           (update :pq pq-pop)
           (assoc  :t  t))))
   (fn ext-update [s e x]
     (let [m (group first second [] x)]
       (if (m :tick)
         (do (assert (empty? (dissoc m :tick)))
             (assert (pq-empty? (:pq s)))
             (assoc s :sl1 (:sl2 s) :t 0))
         (let [t' (+ (:t s) e)]
           (-> (process-object-updates s m step-size t')
               (assoc :t t'))))))
   nil
   (fn [s]
     (if (pq-empty? (:pq s))
       []
       (second (pq-peek (:pq s)))))
   (fn [s]
     (if (pq-empty? (:pq s))
       infinity
       (- (first (pq-peek (:pq s))) (:t s))))))
