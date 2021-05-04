(ns pettomato.devs.lib.random-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [clojure.set :refer [subset?]]
   [pettomato.devs.lib.random :as r]))

(deftest random-test

  (testing "Results are reproducible."
    (is (= (r/with-random-seed 123
             (doall (repeatedly 100 r/rand)))
           (r/with-random-seed 123
             (doall (repeatedly 100 r/rand))))))

  ;; These tests could probably be better. For one, they aren't
  ;; deterministic. Currently, they just check that some basic behavior holds,
  ;; and that the functions aren't returning a bunch of nils.

  (testing "rand: It is highly unlikely the same value will appear twice."
    (is (= 100 (count (r/with-random-seed 123
                        (doall (distinct (repeatedly 100 r/rand))))))))

  (testing "rand-int: All 5 values should appear."
    (is (= 5 (count (r/with-random-seed 123
                      (doall (distinct (repeatedly 100 #(r/rand-int 5)))))))))

  (testing "rand-nth: All 5 values should appear."
    (let [xs [0 1 2 3 4]]
      (is (= 5 (count (r/with-random-seed 123
                        (doall (distinct (repeatedly 100 #(r/rand-nth xs)))))))))))

(deftest shuffle-test

  (let [xs [9 3 1 2 9 8 3 7 7 6]
        ys (r/shuffle xs)]
    (testing "The collections are different."
      (is (not= xs ys)))
    (testing "The values are the same."
      (is (= (frequencies xs)
             (frequencies ys))))))

(deftest random-sample-test

  ;; xs: (vec (repeatedly 100 #(r/rand-int 100)))
  (let [xs [79 63 38 32 17 5 94 50 99 3 44 18 53 80 51 18 96 39 93 61 48 12 39 50 52 75 29 39 28 30 88 3 94 50 45 94 77 40 43 33 42 29 0 34 89 26 25 37 26 69 1 71 34 39 21 21 68 32 23 22 22 5 20 53 76 75 42 46 52 49 43 77 85 64 51 61 90 72 80 70 91 14 42 38 23 42 8 79 66 36 31 79 31 55 47 60 30 21 4 23]
        ys (r/with-random-seed 123
             (doall (r/random-sample 0.3 xs)))]
    (testing "The random sample should be approximately the expected size."
      (is (< 20 (count ys) 40)))
    (testing "The random sample should be a subset of the original collection."
      (is (subset? (set ys) (set xs))))
    (testing "A transducer produces the same results."
      (let [zs (r/with-random-seed 123
                 (into [] (r/random-sample 0.3) xs))]
        (is (= ys zs))))))
