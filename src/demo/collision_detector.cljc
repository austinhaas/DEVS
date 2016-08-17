(ns demo.collision-detector
  (:require
   [clojure.set :refer [union difference]]
   [pt-lib.coll :refer [group]]
   [pt-lib.match :refer [match]]
   [pt-lib.geometry.2D.rect :as r]
   [pt-lib.physics.integration :refer [first-order-euler]]
   [pt-lib.collision.sweep-list :as sl]
   [devs.models :refer [atomic-model]]))

(defn- intervals [s]
  (sl/lookup* (:sl s)))

(defn- nonzero-vels [s]
  (remove (comp zero? second) (:vel s)))

(defn- project-interval [v [a b] h]
  (let [d (first-order-euler v 0 h)] ;; displacement
    [(+ a d) (+ b d)]))

(defn- project-intervals [k->vel k->interval h]
  (for [[k v] k->vel]
    [k (project-interval v (k->interval k) h)]))

(defn- convert-interval
  "Converts an interval given as [lower-bound upper-bound] to [center extent]."
  [[lb ub]]
  (assert (<= lb ub))
  (let [e (/ (- ub lb) 2)]
    [(+ lb e) e]))

(defn- maybe-convert-interval [x]
  (and x (convert-interval x)))

(defn- add-collision-details [sl1 sl2 [a b]]
  {a {:prev (maybe-convert-interval (sl/lookup sl1 a))
      :next (maybe-convert-interval (sl/lookup sl2 a))}
   b {:prev (maybe-convert-interval (sl/lookup sl1 b))
      :next (maybe-convert-interval (sl/lookup sl2 b))}})

(defn- find-time-of-collision [rec]
  (let [[[a {prev-a :prev
             next-a :next}]
         [b {prev-b :prev
             next-b :next}]] (vec rec)]
    (if (and prev-a prev-b)
      (let [[a0 ea] prev-a
            [a1 _ ] next-a
            [b0 eb] prev-b
            [b1 _ ] next-b
            A0      [a0]
            A1      [a1]
            Ea      [ea]
            B0      [b0]
            B1      [b1]
            Eb      [eb]]
        (time-of-collision A0 A1 Ea B0 B1 Eb))
      0)))

(defn- find-time-of-separation [rec]
  (let [[[a {prev-a :prev
             next-a :next}]
         [b {prev-b :prev
             next-b :next}]] (vec rec)]
    (if (and next-a next-b)
      (let [[a0 ea] prev-a
            [a1 _ ] next-a
            [b0 eb] prev-b
            [b1 _ ] next-b
            A0      [a0]
            A1      [a1]
            Ea      [ea]
            B0      [b0]
            B1      [b1]
            Eb      [eb]]
        (time-of-separation A0 A1 Ea B0 B1 Eb))
      0)))

;; Consider using a deferred computation, in case
;; clients don't need the additional info.
;; let [ax (+ a0 (* (- a1 a0) t))
;;      bx (+ b0 (* (- b1 b0) t))]

(defn- delta->events [sl1 sl2 delta]
  (let [add* (for [x (:add delta)]
               (let [rec (add-collision-details sl1 sl2 (vec x))]
                 [(find-time-of-collision rec) [:coll-start rec]]))
        rem* (for [x (:rem delta)]
               (let [rec (add-collision-details sl1 sl2 (vec x))]
                 [(find-time-of-separation rec) [:coll-end rec]]))]
    (->> (concat add* rem*)
         (sort-by first)
         (group first second #{}))))

(defn- scale-time [h events] (map (fn [[t ev]] [(* t h) ev]) events))

(defn- offset-time [t events] (map (fn [[t' ev]] [(+ t t') ev]) events))

;; Warning: Numerical precision may compromise this algorithm. It is
;; assumed that the times of events in e1 and e2 will match exactly.
(defn- merge-events
  "Like merge-sort, but opposite events cancel out."
  [e1 e2]
  (let [smap {:coll-start :coll-end :coll-end :coll-start}
        oppf (fn [[port ev]] [(smap port) ev])]
    (loop [e1  (seq e1)
           e2  (seq e2)
           acc []]
      (cond
        (empty? e1) (into acc e2)
        (empty? e2) (into acc e1)
        :else       (let [[t1 s1] (first e1)
                          [t2 s2] (first e2)]
                      (cond
                        (< t1 t2) (recur (rest e1) e2       (conj acc [t1 s1]))
                        (< t2 t1) (recur e1       (rest e2) (conj acc [t2 s2]))
                        :else     (let [opp1 (set (map oppf s1))
                                        opp2 (set (map oppf s2))
                                        s    (union (difference s1 opp2)
                                                    (difference s2 opp1))]
                                    (if (empty? s)
                                      (recur (rest e1) (rest e2) acc)
                                      (recur (rest e1) (rest e2) (conj acc [t1 s]))))))))))

(defn- project [s t h]
  (let [sl1     (:sl s)
        [sl2 d] (->> (project-intervals (nonzero-vels s) (intervals s) h)
                     (sl/update-interval* sl1))
        events  (->> (delta->events sl1 sl2 d)
                     (scale-time h)
                     (offset-time t)
                     (merge-events (:events s)))]
    (assoc s :sl sl2 :events events :t t)))

(defn- rollback [s t h]
  (let [sl1     (:sl s)
        [sl2 d] (->> (project-intervals (nonzero-vels s) (intervals s) (- h))
                     (sl/update-interval* sl1))
        events  (->> (delta->events sl1 sl2 d)
                     (scale-time h)
                     reverse
                     (offset-time (- h))
                     (offset-time t)
                     (merge-events (:events s)))]
    (assoc s :sl sl2 :events events :t t)))

(defn ingest [s events t]
  (let [sl1         (:sl s)
        [vel sl2 d] (reduce (fn [[vel sl d] ev]
                              (match ev
                                [:add [k p v e]] (let [[sl' d'] (sl/add-interval sl k [(- p e) (+ p e)])]
                                                   [(assoc vel k v) sl' (merge-with into d d')])
                                [:rem k]         (let [[sl' d'] (sl/rem-interval sl k)]
                                                   [(dissoc vel k) sl' (merge-with into d d')])
                                [:vel [k v]]     [(assoc vel k v) sl d]))
                            [(:vel s) sl1 {}]
                            events)
        events'    (->> (delta->events sl1 sl2 d)
                        (scale-time 0)
                        (offset-time t)
                        (merge-events (:events s)))]
    (assoc s :vel vel :sl sl2 :events events' :t t)))

(defn collision-detector [step-size]
  (atomic-model
   {:vel    {}
    :sl     sl/empty-sweep-list
    :events []
    :t      0}
   (fn int-update [s]
     (if (seq (:events s))
       (let [[t ev] (first (:events s))]
         (-> s
             (update :events rest)
             (assoc  :t      t)))
       (project s 0 step-size)))
   (fn ext-update [s e x]
     (let [t         (+ (:t s) e)
           sigma     (- step-size t)
           port->ev* (group first second [] x)
           ;; vel depends on :add and :rem, too.

           vel'      (reduce (fn [m [k v]] (assoc m k v))
                             (:vel s)
                             (port->ev* :vel))]
       (-> s
           (rollback t sigma)
           (assoc :vel vel')
           (ingest x t)
           (project t sigma))))
   nil
   (fn [s]
     (if (seq (:events s))
       (second (first (:events s)))
       []))
   (fn [s]
     (if (seq (:events s))
       (- (ffirst (:events s)) (:t s))
       (- step-size (:t s))))))
