(ns demo.db-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq?]]
   [devs.atomic-simulator :refer [atomic-simulator]]
   [devs.immediate-system :refer [immediate-system]]
   [demo.publisher :refer [publisher]]))

(defn- normalize-results [results]
  (mapv (fn [[t [port [query val]]]]
          [t [port [query (set val)]]])
        results))

(def f #(-> (atomic-simulator (publisher))
            (immediate-system 0 1000 %)
            normalize-results))

(deftest db-subscription-tests
  (is (eq? (f [[0  [[:sub :q] {:x odd?}]]
               [10 [:pub {:x 1}]]
               [20 [:pub {:x 2}]]
               [30 [:pub {:x 3}]]
               [40 [[:unsub :q] {:x odd?}]]
               [50 [:pub {:x 5}]]])
           [[10 [[:sub-response :q] [{:x odd?} #{[:x 1]}]]]
            [30 [[:sub-response :q] [{:x odd?} #{[:x 3]}]]]])))
