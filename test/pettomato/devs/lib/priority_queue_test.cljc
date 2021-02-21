(ns pettomato.devs.lib.priority-queue-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.priority-queue :as pq]))

(deftest priority-queue-tests
  (is (pq/empty? (pq/priority-queue)))

  (testing "insert"
    (is (= [[1 #{:a :d}]
            [2 #{:b}]
            [3 #{:c}]]
           (-> (pq/priority-queue)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :d)
               (pq/->seq)))))

  (testing "delete"
    (is (= [[1 #{:a}]
            [3 #{:c}]]
           (-> (pq/priority-queue)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/delete 2 :b)
               (pq/->seq)))))

  (testing "delete, item not found"
    (is (= [[1 #{:a}]
            [2 #{:b}]
            [3 #{:c}]]
           (-> (pq/priority-queue)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/delete 2 :x)
               (pq/->seq)))))

  (testing "change-priority"
    (is (= [[2 #{:b}]
            [3 #{:c}]
            [4 #{:a}]]
           (-> (pq/priority-queue)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/change-priority 1 :a 4)
               (pq/->seq)))))

  (testing "peek-key"
    (is (= 1
           (-> (pq/priority-queue)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :x)
               (pq/peek-key)))))

  (testing "peek"
    (is (= #{:a :x}
           (-> (pq/priority-queue)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :x)
               (pq/peek)))))

  (testing "pop"
    (is (= [[2 #{:b}]
            [3 #{:c}]]
           (-> (pq/priority-queue)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :x)
               (pq/pop)
               (pq/->seq))))))
