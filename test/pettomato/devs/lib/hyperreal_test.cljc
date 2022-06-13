(ns pettomato.devs.lib.hyperreal-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]))

(deftest hyperreal-tests

  (testing "constructor"

    (is (h/= h/zero (*R 0 0) (*R 0)))

    (is (h/= h/epsilon (*R 0 1))))

  (testing "hyperreal?"

    (is (true? (h/hyperreal? h/zero)))

    (is (true? (h/hyperreal? h/epsilon)))

    (is (true? (h/hyperreal? h/infinity)))

    (is (true? (h/hyperreal? (*R 1 2))))

    (is (false? (h/hyperreal? 27))))

  (testing "standard"

    (is (= 0 (h/standard h/zero)))

    (is (= 1 (h/standard (*R 1 2))))

    (is (= ##Inf (h/standard h/infinity))))

  (testing "="

    (is (h/= h/zero))

    (is (h/= h/zero h/zero))

    (is (h/= (*R 1 2) (*R 1 2) (*R 1 2)))

    (is (false? (h/= h/infinity (*R 0 1)))))

  (testing "<"

    (is (h/< h/zero))

    (is (h/< (*R -1)
             (*R -1 1)
             h/zero
             h/epsilon
             (*R 1 -1)
             (*R 1)
             (*R 1 1)
             h/infinity)))

  (testing "<="

    (is (h/<= h/zero))

    ;; Same arguments as `<` test.
    (is (h/<= (*R -1)
              (*R -1 1)
              h/zero
              h/epsilon
              (*R 1 -1)
              (*R 1)
              (*R 1 1)
              h/infinity))

    ;; Same args as above, but every arg twice.
    (is (h/<= (*R -1)
              (*R -1)
              (*R -1 1)
              (*R -1 1)
              h/zero
              h/zero
              h/epsilon
              h/epsilon
              (*R 1 -1)
              (*R 1 -1)
              (*R 1)
              (*R 1)
              (*R 1 1)
              (*R 1 1)
              h/infinity
              h/infinity)))

  (testing "comparator"

    (is (=  0 (h/comparator h/zero     h/zero)))
    (is (= -1 (h/comparator h/zero     h/epsilon)))
    (is (=  1 (h/comparator h/infinity h/epsilon)))

    (let [vals-in-order [(*R -10 500)
                         (*R 0 -100)
                         (*R 0 0)
                         (*R 0 10)
                         (*R 10 -10000)
                         h/infinity]]
      (is (every? (fn [[a b]] (h/= a b))
                  (map vector
                       vals-in-order
                       (->> (shuffle vals-in-order)
                            (apply sorted-set-by h/comparator)
                            seq))))))

  (testing "min"

    (is (= h/zero (h/min h/zero)))

    (is (= h/zero (h/min h/infinity h/epsilon h/zero))))

  (testing "max"

    (is (= h/zero (h/max h/zero)))

    (is (= h/infinity (h/max h/epsilon h/infinity h/zero))))

  (testing "pos?"

    (is (false? (h/pos? (*R 0 -1))))

    (is (false? (h/pos? h/zero)))

    (is (h/pos? h/epsilon))

    (is (h/pos? h/infinity))

    (is (h/pos? (*R 1 -999))))

  (testing "infinite?"

    (is (false? (h/infinite? h/zero)))

    (is (false? (h/infinite? h/epsilon)))

    (is (h/infinite? h/infinity))

    (is (h/infinite? h/positive-infinity))

    (is (h/infinite? h/negative-infinity))))
