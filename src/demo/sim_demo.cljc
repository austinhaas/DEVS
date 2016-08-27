(ns demo.sim-demo
  (:require
   [clojure.core.async :as async :refer [chan go <! timeout close! >! onto-chan]]
   [pt-lib.coll :refer [group]]
   [pt-lib.geometry.2D.rect :as r]
   [pt-lib.geometry.2D.point :as pt]
   [pt-lib.geometry.2D.vect :as v]
   [pt-lib.number :refer [infinity]]
   [pt-lib.physics.integration :refer [verlet-2d]]
   [devs.models :refer [atomic-model executive-model network-model register connect]]
   [demo.db :refer [db]]
   [demo.publisher :refer [publisher]]
   [demo.collision-detector :refer [collision-detector]]
   [devs.network-simulator :refer [network-simulator]]
   [devs.immediate-system :refer [immediate-system]]
   [devs.real-time-system :refer [real-time-system]]))

(defn clock [h]
  (atomic-model
   nil
   (constantly nil)
   (constantly nil)
   nil
   (fn output [s] [[:tick nil]])
   (constantly h)))

(defn collision-detector-subscriber
  "Initializes a subscription for collision-detector and parses
  incoming messages for it."
  []
  (atomic-model
   {:sigma  0
    :output [[[:sub :db] [{:pos1 (constantly true) :pos2 (constantly true)} {:pos1 identity :pos2 identity}]]
             [[:sub :pub] {:type :tick}]]}
   (fn int-update [s] (assoc s :sigma infinity :output []))
   (fn ext-update [s e x]
     (let [port->ev* (group first second [] x)
           db   (port->ev* [:sub-response :db])
           pub  (port->ev* [:sub-response :pub])
           out1 (for [[sub res] db, y res]
                  (cond
                    (empty? (:old y)) (let [{:keys [id collision-group shape pos2]} (:new y)]
                                        [:insert [[id collision-group] (r/move-to shape pos2)]])
                    (empty? (:new y)) (let [{:keys [id collision-group shape pos2]} (:old y)]
                                        [:delete [[id collision-group] (r/move-to shape pos2)]])
                    :else             (let [{:keys [id collision-group shape pos2]} (:new y)]
                                        [:modify [[id collision-group] (r/move-to shape pos2)]])))
           out2 (for [x pub] [:tick nil])]
       (assoc s
              :output (concat out1 out2)
              :sigma 0)))
   nil
   :output
   :sigma))

(defn collision-detector-network [h]
  (network-model
   :cdet-net
   (executive-model
    (-> {}
        (register :cdet (collision-detector h (constantly true)))
        (register :sub  (collision-detector-subscriber))
        (connect :N    [:sub-response :db]  :sub  [:sub-response :db])
        (connect :N    [:sub-response :pub] :sub  [:sub-response :pub])
        (connect :sub  [:sub :db]    :N    [:sub :db])
        (connect :sub  [:sub :pub]   :N    [:sub :pub])
        (connect :sub  :insert       :cdet :insert)
        (connect :sub  :delete       :cdet :delete)
        (connect :sub  :modify       :cdet :modify)
        (connect :sub  :tick         :cdet :tick)
        (connect :cdet :coll-start   :N    :coll-start)
        (connect :cdet :coll-end     :N    :coll-end))
    nil nil nil nil
    (constantly infinity))))

(defn separate-objects [akey apos1 apos2 amass ashape
                        bkey bpos1 bpos2 bmass bshape]
  (let [a0 (r/move-by ashape apos1)
        a1 (r/move-by ashape apos2)
        b0 (r/move-by bshape bpos1)
        b1 (r/move-by bshape bpos2)

        sv (r/separate a0 a1 b0 b1)

        sv1 (v/mult sv (/ bmass (+ amass bmass)))
        sv2 (v/reverse (v/mult sv (/ amass (+ amass bmass))))

        apos' (pt/add apos2 sv1)
        bpos' (pt/add bpos2 sv2)]

    [apos' bpos']))

(defn collision-responder []
  (let [f1 (fn f1 [s x]
             (reduce (fn [s [q r]]
                       (let [[a b] (:pair r)]
                         ;; This is a very inefficient query!
                         (update s :output conj [[:query :db] {:id (fn [x] (contains? #{a b} x))}])))
                     s
                     x))
        f2 (fn f2 [s x]
             (reduce (fn [s [q r]]
                       (assert (= (count r) 2))
                       (let [[a b] r
                             [apos' bpos'] (separate-objects (:id a) (:pos1 a) (:pos2 a) (:mass a) (:shape a)
                                                             (:id b) (:pos1 b) (:pos2 b) (:mass b) (:shape b))]
                         (update s :output conj
                                 [:modify [{:pos2 apos'} {:id (:id a)}]]
                                 [:modify [{:pos2 bpos'} {:id (:id b)}]])))
                     s
                     x))]
   (atomic-model
    {:output [[[:sub :pub] {:type :coll-start}]]
     :sigma  0}
    (fn int-update [s] (assoc s :output [] :sigma infinity))
    (fn ext-update [s e x]
      (let [port->ev* (group first second [] x)]
        (-> s
            (f1 (port->ev* [:sub-response :pub]))
            (f2 (port->ev* [:query-response :db]))
            (assoc :sigma 0))))
    nil
    :output
    :sigma)))

(defn game-object [id pos vel mass shape collision-group h]
  (atomic-model
   (let [pos1 pos
         pos2 (pt/add pos vel)]
     {:pos1   pos1
      :pos2   pos2
      :output [[[:sub :pub] {:type :tick}]
               [[:sub :db] [{:id id} {:pos2 identity}]]
               [:insert {:id id :shape shape :pos1 pos1 :pos2 pos2 :mass mass :collision-group collision-group}]]
      :sigma  0})
   (fn int-update [s] (assoc s :output [] :sigma infinity))
   (fn ext-update [s e x]
     (reduce (fn [s [port val]]
               (case port
                 [:sub-response :pub] (let [{:keys [pos1 pos2]} s
                                            pos1' pos2
                                            pos2' (verlet-2d [0 0] pos1 pos2 h)]
                                        (assoc s
                                               :pos1   pos1'
                                               :pos2   pos2'
                                               :output [[:modify [{:pos1 pos1' :pos2 pos2'} {:id id}]]]
                                               :sigma  0))
                 [:sub-response :db] (let [[q r] val]
                                       (assert (= (count r) 1))
                                       (assoc s :pos2 (:pos2 (:new (first r)))))))
             s
             x))
   nil
   :output
   :sigma))

(defn display []
  (atomic-model
   {:output [[[:sub :db] [{:pos1 (constantly true)} {:pos1 identity}]]]
    :sigma  0}
   (fn int-update [s] (assoc s :sigma infinity :output []))
   (fn ext-update [s e x]
     (doseq [[port [q r]] x]
       (doseq [s r]
         (println ">" (:id (:new s)) (mapv float (:pos1 (:new s))))))
     s)
   nil
   :output
   :sigma))

(defn exec [name h]
  (executive-model
   (-> {}
       (register :clock (clock h))
       (register :pub   (publisher))
       (register :db    (db :id))
       (register :cdet  (collision-detector-network h))
       (register :cres  (collision-responder))
       (register :obj-1 (game-object :obj-1 [0 0] [1 0] 1 [0 0 4 4] :alpha h))
       (register :obj-2 (game-object :obj-2 [6 0] [0 0] 1 [0 0 4 4] :alpha h))
       (register :disp  (display))

       (connect :N    :insert :db :insert)
       (connect :N    :modify :db :modify)
       (connect :N    [:query :q] :db  [:query :q])
       (connect :db   [:query-response :q] :N :out)
       (connect :cdet [:sub :db] :db [:sub :cdet])
       (connect :cdet [:sub :pub] :pub [:sub :cdet])

       (connect :db   [:sub-response :cdet] :cdet [:sub-response :db])
       (connect :pub  [:sub-response :cdet] :cdet [:sub-response :pub])

       (connect :cdet :coll-start :pub :pub (fn [x] {:type :coll-start :pair x}))
       (connect :cdet :coll-end   :pub :pub (fn [x] {:type :coll-end   :pair x}))

       (connect :clock :tick :pub :pub (constantly {:type :tick}))

       (connect :obj-1 [:sub :pub] :pub [:sub :obj-1])
       (connect :pub [:sub-response :obj-1] :obj-1 [:sub-response :pub])
       (connect :obj-1 [:sub :db] :db [:sub :obj-1])
       (connect :db  [:sub-response :obj-1] :obj-1 [:sub-response :db])
       (connect :obj-1 :insert :db :insert)
       (connect :obj-1 :modify :db :modify)

       (connect :obj-2 [:sub :pub] :pub [:sub :obj-2])
       (connect :pub [:sub-response :obj-2] :obj-2 [:sub-response :pub])
       (connect :obj-2 [:sub :db] :db [:sub :obj-2])
       (connect :db  [:sub-response :obj-2] :obj-2 [:sub-response :db])
       (connect :obj-2 :insert :db :insert)
       (connect :obj-2 :modify :db :modify)

       (connect :cres [:sub :pub] :pub [:sub :cres])
       (connect :pub [:sub-response :cres] :cres [:sub-response :pub])
       (connect :cres :modify     :db   :modify)
       (connect :cres [:query :db] :db [:query :cres])
       (connect :db   [:query-response :cres] :cres [:query-response :db])

       (connect :disp [:sub :db] :db [:sub :disp])
       (connect :db   [:sub-response :disp] :disp [:sub-response :db]))
   nil nil nil nil
   (constantly infinity)))

(defn network [] (network-model :net (exec :net 1000)))

#_
(clojure.pprint/pprint
 (immediate-system (network-simulator (network)) 0 20000 []))

#_
(do
  (def sim (network-simulator (network)))
  (def chan-in  (chan 100))
  (def chan-out (chan 100))

  (real-time-system sim 0 chan-in chan-out)

  (go (loop []
        (if-let [v (<! chan-out)]
          (do (println (format "[%s] %s" (first v) (second v)))
              (recur))
          (println 'done)))))

#_
(onto-chan chan-in [[[:add :a] [0 1 1]]
                    [[:add :b] [5 -1 1]]]
           false)

#_(close! chan-in)
