(ns demo.collision-detector
  (:require
   [pt-lib.match :refer [match]]
   [pt-lib.math :refer [abs]]
   [pt-lib.number :refer [infinity]]
   [pt-lib.physics.integration :refer [euler-step]]
   [pt-lib.collision.sweep-list-interval :as sli]
   [devs.atomic-model :refer [atomic-model]]))

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

(defn- sli->events [sli h]
  (letfn [(convert [a]
            (let [[alr aur] (sli/lookup-rev sli a)
                  [alf auf] (sli/lookup-fwd sli a)
                  aer       (/ (- aur alr) 2)
                  aef       (/ (- auf alf) 2)
                  A0        [(+ alr aer)]
                  A1        [(+ alf aef)]
                  Ea        [aer]]
              (assert (= aer aef) "time-of-collision/separation assume ext doesn't change.")
              [A0 A1 Ea]))]
    (let [add* (for [[a b] (map vec (sli/intersections sli))]
                 (if (and (sli/lookup-rev sli a)
                          (sli/lookup-rev sli b))
                   (let [[A0 A1 Ea] (convert a)
                         [B0 B1 Eb] (convert b)
                         t          (time-of-collision A0 A1 Ea B0 B1 Eb)]

                     [t [:coll-start #{a b}]])
                   [0 [:coll-start #{a b}]]))
          rem* (for [[a b] (map vec (sli/separations sli))]
                 (if (and (sli/lookup-fwd sli a)
                          (sli/lookup-fwd sli b))
                   (let [[A0 A1 Ea] (convert a)
                         [B0 B1 Eb] (convert b)
                         t          (time-of-separation A0 A1 Ea B0 B1 Eb)]
                     [t [:coll-end #{a b}]])
                   [0 [:coll-end #{a b}]]))]
      (->> (concat add* rem*)
           (sort-by first)
           ;; relative -> absolute time
           (map (fn [[t ev]] [(* t h) ev]))
           ;; absolute -> delta time
           (reduce (fn [[acc x] [t ev]] [(conj acc [(- t x) ev]) t]) [[] 0])
           first))))

(defn- sli-move* [sli i->vel h]
  (->> i->vel
       (remove (comp zero? second))
       (map (fn [[i v]]
              (let [[a b] (sli/lookup-fwd sli i)
                    d     (euler-step v 0 h)]
                [i [(+ a d) (+ b d)]])))
       (sli/update-interval* sli)))

(defn collision-detector [step-size]
  (atomic-model
   {:vel    {}
    :sli    sli/empty-sweep-list-interval
    :output []
    :sigma  step-size}
   (fn int-update [s]
     (if (seq (:output s))
       (-> s
           (update :output rest)
           (update :sigma  - (ffirst (:output s))))
       (let [sli (-> (:sli s)
                     sli/collapse-fwd
                     (sli-move* (:vel s) step-size))
             ev* (sli->events sli step-size)]
         (assoc s :sli sli :output ev* :sigma step-size))))
   (fn ext-update [s e x]
     ;; 1. Start at previously known positions.
     ;; 2. Project to the current time.
     ;; 3. Incorporate new data.
     ;; 4. Project to the next int-update time.
     (let [sigma       (- (:sigma s) e)
           vel         (:vel s)
           sli1        (sli/collapse-rev (:sli s))
           sli2        (sli-move* sli1 vel (- step-size sigma)) ;; There shouldn't be any collision events here.
           [vel' sli3] (reduce (fn [[vel sli] ev]
                                 (match ev
                                   [[:add k] [p v e]] [(assoc  vel k v) (sli/add-interval sli k [(- p e) (+ p e)])]
                                   [[:rem k] nil]     [(dissoc vel k)   (sli/rem-interval sli k)]
                                   [[:vel k] v]       [(assoc  vel k v) sli]))
                               [vel (sli/collapse-fwd sli2)]
                               x)
           ev1*        (sli->events sli3 0)
           sli4        (sli-move* (sli/collapse-fwd sli3) vel' sigma)
           ev2*        (sli->events sli4 sigma)]
       (assoc s :vel vel' :sli sli4 :sigma sigma :output (concat ev1* ev2*))))
   nil
   (fn [s] [(second (first (:output s)))])
   (fn [s]
     (if (seq (:output s))
       (ffirst (:output s))
       (:sigma s)))))
