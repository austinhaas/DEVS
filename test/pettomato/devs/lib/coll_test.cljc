(ns pettomato.devs.lib.coll-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.coll :refer [queue prune]]))

(deftest prune-test

  (testing "passing empty keys has no effect"
    (is (= {:x #{1}}
           (prune {:x #{1}} []))))

  (testing "branches with empty leaves are pruned."
    (is (= {:x #{1}}
           (prune {:x #{1} :y {:z []}} [:y :z])))))

(deftest queue-test

  (testing "basic behavior"
    (is (= 1
           (-> queue
               (conj 1)
               (conj 2)
               (conj 3)
               peek))))

  (testing "Prints without error."
    (is (string? (with-out-str (pr (-> queue
                                       (conj 1)
                                       (conj 2)
                                       (conj 3))))))))
