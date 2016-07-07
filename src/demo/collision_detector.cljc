(ns demo.collision-detector
  (:require
   [pt-lib.match :refer [match]]
   [pt-lib.physics.integration :refer [euler-step]]
   [pt-lib.collision.collision :refer [time-of-collision time-of-separation]]
   [pt-lib.collision.sweep-list :as sl]
   [devs.models :refer [atomic-model]]))

(defn- convert-segment
  "Converts a segment given as [lower-bound upper-bound] to [center extent]."
  [[lb ub]]
  (assert (<= lb ub))
  (let [e (/ (- ub lb) 2)]
    [(+ lb e) e]))

(defn- sl->events [sl1 sl2 delta]
  (let [add* (for [[a b] (map vec (:add delta))]
               (let [A  (sl/lookup sl1 a)
                     B  (sl/lookup sl1 b)
                     A' (sl/lookup sl2 a)
                     B' (sl/lookup sl2 b)]
                 (if (and A B)
                   (let [[a0 ea] (convert-segment A)
                         [a1 _ ] (convert-segment A')
                         [b0 eb] (convert-segment B)
                         [b1 _ ] (convert-segment B')
                         A0      [a0]
                         A1      [a1]
                         Ea      [ea]
                         B0      [b0]
                         B1      [b1]
                         Eb      [eb]
                         t       (time-of-collision A0 A1 Ea B0 B1 Eb)]
                     ;; Consider using a deferred computation, in case
                     ;; clients don't need the additional info.
                     (let [ax (+ a0 (* (- a1 a0) t))
                           bx (+ b0 (* (- b1 b0) t))]
                       [t [:coll-start {a [ax ea] b [bx eb]}]]))
                   [0 [:coll-start {a (convert-segment A')
                                    b (convert-segment B')}]])))
        rem* (for [[a b] (map vec (:rem delta))]
               (let [A  (sl/lookup sl1 a)
                     B  (sl/lookup sl1 b)
                     A' (sl/lookup sl2 a)
                     B' (sl/lookup sl2 b)]
                 (if (and A' B')
                   (let [[a0 ea] (convert-segment A)
                         [a1 _ ] (convert-segment A')
                         [b0 eb] (convert-segment B)
                         [b1 _ ] (convert-segment B')
                         A0      [a0]
                         A1      [a1]
                         Ea      [ea]
                         B0      [b0]
                         B1      [b1]
                         Eb      [eb]
                         t       (time-of-separation A0 A1 Ea B0 B1 Eb)]
                     (let [ax (+ a0 (* (- a1 a0) t))
                           bx (+ b0 (* (- b1 b0) t))]
                       [t [:coll-end {a [ax ea] b [bx eb]}]]))
                   [0 [:coll-end {a (convert-segment A)
                                  b (convert-segment B)}]])))]
    (->> (concat add* rem*)
         (sort-by first))))

(defn- sl-move* [sl i->vel h]
  (->> i->vel
       (remove (comp zero? second))
       (map (fn [[i v]]
              (let [[a b] (sl/lookup sl i)
                    d     (euler-step v 0 h)] ;; displacement
                [i [(+ a d) (+ b d)]])))
       (sl/update-interval* sl)))

(defn- aggregate-events [events]
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
(defn- merge-events [e1 e2]
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
                          aggregate-events)]
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
                              (map (fn [[t' ev]] [(+ t t') ev]))
                              aggregate-events)
           events3       (->> (sl->events sl2 sl3 d3)
                              ;; relative -> absolute time
                              (map (fn [[t ev]] [(* t 0) ev]))
                              ;; Offset time.
                              (map (fn [[t' ev]] [(+ t t') ev]))
                              aggregate-events)
           events4       (->> (sl->events sl3 sl4 d4)
                              ;; relative -> absolute time
                              (map (fn [[t ev]] [(* t sigma) ev]))
                              ;; Offset time.
                              (map (fn [[t' ev]] [(+ t t') ev]))
                              aggregate-events)
           events'       (reduce merge-events [events1 events2 events3 events4])]
       (assoc s :vel vel' :sl sl4 :t t :events events')))
   nil
   (fn [s]
     (if (seq (:events s))
       (second (first (:events s)))
       []))
   (fn [s]
     (if (seq (:events s))
       (- (ffirst (:events s)) (:t s))
       (- step-size (:t s))))))
