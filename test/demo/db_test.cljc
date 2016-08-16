(ns demo.db-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq?]]
   [devs.atomic-simulator :refer [atomic-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [demo.db :refer [db]]))

(defn foo []
 (clojure.pprint/pprint
  (fast-as-possible-system
   (atomic-simulator (db :id))
   0
   400
   [[0  [:insert {:id :foo :a 0 :b 1}]]
    [0  [:insert {:id :dif :x "banana"}]]
    [10 [[:sub :q] [{} {:a even? :b odd?}]]]
    [20 [:insert {:id :bar :a 0 :b 2}]]
    [30 [:insert {:id :cat :a 0 :b 2}]]
    [40 [:modify {:a 1 :b 2} {:id :foo}]]
    [50 [[:sub :q] [{} {:x string?}]]]
;;    [55 [[:unsub :q] [{} {:a even? :b odd?}]]]
    [60 [:delete {:id :foo}]]
    ]))


  )
