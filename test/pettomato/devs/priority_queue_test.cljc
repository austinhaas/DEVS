(ns pettomato.devs.priority-queue-test
  (:require
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.priority-queue :as pq]))

(deftest priority-queue-tests
  (is (pq/empty? (pq/init)))

  (testing "insert"
    (is (= [[1 #{:a :d}]
            [2 #{:b}]
            [3 #{:c}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :d)
               (pq/->seq)))))

  (testing "insert*"
    (is (= [[1 #{:a :d}]
            [2 #{:b}]
            [3 #{:c}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert* 1 [:a :d])
               (pq/insert 3 :c)
               (pq/->seq)))))

  (testing "delete"
    (is (= [[1 #{:a}]
            [3 #{:c}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/delete 2 :b)
               (pq/->seq)))))

  (testing "delete, item not found"
    (is (= [[1 #{:a}]
            [2 #{:b}]
            [3 #{:c}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/delete 2 :x)
               (pq/->seq)))))

  (testing "delete*"
    (is (= [[3 #{:c}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 2 :a)
               (pq/insert 3 :c)
               (pq/delete* 2 [:a :b :x])
               (pq/->seq)))))

  (testing "change-priority"
    (is (= [[2 #{:b}]
            [3 #{:c}]
            [4 #{:a}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/change-priority 1 :a 4)
               (pq/->seq)))))

  (testing "change-priority*"
    (is (= [[4 #{:a}]
            [5 #{:b}]
            [6 #{:c}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/change-priority* [[2 :b 5]
                                     [1 :a 4]
                                     [3 :c 6]])
               (pq/->seq)))))

  (testing "peek-key"
    (is (= 1
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :x)
               (pq/peek-key)))))

  (testing "peek"
    (is (= #{:a :x}
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :x)
               (pq/peek)))))

  (testing "pop"
    (is (= [[2 #{:b}]
            [3 #{:c}]]
           (-> (pq/init)
               (pq/insert 2 :b)
               (pq/insert 1 :a)
               (pq/insert 3 :c)
               (pq/insert 1 :x)
               (pq/pop)
               (pq/->seq)))))

  )
