(ns demo.collision-detector
  (:require
   [pt-lib.match :refer [match]]
   [pt-lib.math :refer [abs]]
   [pt-lib.number :refer [infinity]]
   [pt-lib.physics.integration :refer [euler-step]]
   [pt-lib.collision.sweep-list :as sl]
   [devs.models :refer [atomic-model]]))

;; http://www.gamasutra.com/view/feature/131790/simple_intersection_tests_for_games.php?page=3

(defn- overlaps? [A Ea B Eb]
  ;; A is the center. Ea is the extent in each axis.
  (every? true? (map (fn [a ea b eb] (<= (abs (- b a)) (+ ea eb))) A Ea B Eb)))

(defn- time-of-collision% [A A' Ea B B' Eb]
  (let [;; Displacement of A and B.
        va (- A' A)
        vb (- B' B)
        ;; The problem is solved in A's frame of reference.
        v  (- vb va)
        a0 (- A Ea)
        a1 (+ A Ea)
        b0 (- B Eb)
        b1 (+ B Eb)
        l  (cond
             (and (< a1 b0) (< v 0)) (/ (- a1 b0) v)
             (and (< b1 a0) (> v 0)) (/ (- a0 b1) v)
             :else                   0)
        u  (cond
             (and (> b1 a0) (< v 0)) (/ (- a0 b1) v)
             (and (> a1 b0) (> v 0)) (/ (- a1 b0) v)
             :else                   1)]
    [l u]))

(defn- time-of-collision [A0 A1 Ea B0 B1 Eb]
  (let [xs (map time-of-collision% A0 A1 Ea B0 B1 Eb)
        ;; Possible first and last times of overlap.
        u0 (apply max (map first  xs))
        u1 (apply min (map second xs))]
    ;; They could have only collided if the first time of overlap
    ;; occured before the last time of overlap.
    (if (<= u0 u1) u0 -1)))

(defn- time-of-separation [A0 A1 Ea B0 B1 Eb]
  (let [xs (map time-of-collision% A0 A1 Ea B0 B1 Eb)
        ;; Possible first and last times of overlap.
        u0 (apply max (map first  xs))
        u1 (apply min (map second xs))]
    ;; They could have only collided if the first time of overlap
    ;; occured before the last time of overlap.
    (if (<= u0 u1) u1 -1)))

(defn- sl->events [sl1 sl2 delta]
  (letfn [(convert [a]
            (let [[alr aur] (sl/lookup sl1 a)
                  [alf auf] (sl/lookup sl2 a)
                  aer       (/ (- aur alr) 2)
                  aef       (/ (- auf alf) 2)
                  A0        [(+ alr aer)]
                  A1        [(+ alf aef)]
                  Ea        [aer]]
              ;;(assert (= aer aef) "time-of-collision/separation assume ext doesn't change.")
              [A0 A1 Ea]))]
    (let [add* (for [[a b] (map vec (:add delta))]
                 (if (and (sl/lookup sl1 a)
                          (sl/lookup sl1 b))
                   (let [[A0 A1 Ea] (convert a)
                         [B0 B1 Eb] (convert b)
                         t          (time-of-collision A0 A1 Ea B0 B1 Eb)]
                     [t [:coll-start #{a b}]])
                   [0 [:coll-start #{a b}]]))
          rem* (for [[a b] (map vec (:rem delta))]
                 (if (and (sl/lookup sl2 a)
                          (sl/lookup sl2 b))
                   (let [[A0 A1 Ea] (convert a)
                         [B0 B1 Eb] (convert b)
                         t          (time-of-separation A0 A1 Ea B0 B1 Eb)]
                     [t [:coll-end #{a b}]])
                   [0 [:coll-end #{a b}]]))]
      (->> (concat add* rem*)
           (sort-by first)))))

(defn- absolute->delta-time [events]
  (->> events
       (reduce (fn [[acc x] [t ev]] [(conj acc [(- t x) ev]) t]) [[] 0])
       first))

(defn- sl-move* [sl i->vel h]
  (->> i->vel
       (remove (comp zero? second))
       (map (fn [[i v]]
              (let [[a b] (sl/lookup sl i)
                    d     (euler-step v 0 h)] ;; displacement
                [i [(+ a d) (+ b d)]])))
       (sl/update-interval* sl)))

(defn aggregate-events [events]
  (reduce (fn [acc [t ev]]
            (if (empty? acc)
              (conj acc [t #{ev}])
              (let [[t' s] (peek acc)]
                (if (= t t')
                  (conj (pop acc) [t (conj s ev)])
                  (conj acc [t #{ev}])))))
          []
          events))

;; Numerical precision may compromise this algorithm.
(defn merge-events [e1 e2]
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
                      :else     (let [smap {:coll-start :coll-end :coll-end :coll-start}
                                      s1'  (->> s1
                                                (map #(replace smap %))

                                                set)
                                      s2'  (set (map #(replace smap %) s2))
                                      s    (clojure.set/union
                                            (clojure.set/difference s1 s2')
                                            (clojure.set/difference s2 s1'))]
                                  (if (empty? s)
                                    (recur (rest e1) (rest e2) acc)
                                    (recur (rest e1) (rest e2) (conj acc [t1 s])))))))))

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
       (let [sl1     (:sl s)
             [sl2 d] (sl-move* sl1 (:vel s) step-size)
             events  (->> (sl->events sl1 sl2 d)
                          ;; relative -> absolute time
                          (map (fn [[t ev]] [(* t step-size) ev]))
                          vec)]
         (assoc s :sl sl2 :events events :t 0))))
   (fn ext-update [s e x]
     (let [vel           (:vel s)
           sl1           (:sl s)
           t             (+ (:t s) e)
           sigma         (- step-size t)
           ;; Rollback from future to current time.
           [sl2 d2]      (sl-move* sl1 vel (- sigma))
           ;; Incorporate new data.
           [vel' sl3 d3] (reduce (fn [[vel sl d] ev]
                                   (match ev
                                     [:add [k p v e]] (let [[sl' d'] (sl/add-interval sl k [(- p e) (+ p e)])]
                                                        [(assoc vel k v) sl' (merge-with into d d')])
                                     [:rem k]         (let [[sl' d'] (sl/rem-interval sl k)]
                                                        [(dissoc vel k) sl' (merge-with into d d')])
                                     [:vel [k v]]       [(assoc vel k v) sl d]))
                                 [vel sl2 {}]
                                 x)
           ;; Project to the next int-update time.
           [sl4 d4]      (sl-move* sl3 vel' sigma)
           events1       (:events s)
           events2       (->> (sl->events sl1 sl2 d2)
                              ;; relative -> absolute time
                              (map (fn [[t ev]] [(* t sigma) ev]))
                              reverse
                              (map (fn [[t ev]] [(- sigma t) ev]))
                              ;; Offset time.
                              (map (fn [[t' ev]] [(+ t t') ev])))
           events3       (->> (sl->events sl2 sl3 d3)
                              ;; relative -> absolute time
                              (map (fn [[t ev]] [(* t 0) ev]))
                              ;; Offset time.
                              (map (fn [[t' ev]] [(+ t t') ev])))
           events4       (->> (sl->events sl3 sl4 d4)
                              ;; relative -> absolute time
                              (map (fn [[t ev]] [(* t sigma) ev]))
                              ;; Offset time.
                              (map (fn [[t' ev]] [(+ t t') ev])))
           ;; Aggregate events occuring at the same time.
           ;;   This is probably how events should be organized all the time.
           events1'      (aggregate-events events1)
           events2'      (aggregate-events events2)
           events3'      (aggregate-events events3)
           events4'      (aggregate-events events4)
           events'       (reduce merge-events [events1' events2' events3' events4'])
           ;; De-aggregate events.
           events''      (for [[t s] events', ev s] [t ev])]
       (assoc s :vel vel' :sl sl4 :t t :events events'')))
   nil
   (fn [s]
     (if (seq (:events s))
       [(second (first (:events s)))]
       []))
   (fn [s]
     (if (seq (:events s))
       (- (ffirst (:events s)) (:t s))
       (- step-size (:t s))))))
