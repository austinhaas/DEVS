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

    (is (h/= h/epsilon (*R 0 1)))

    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"standard part cannot be infinite"
                          (*R ##Inf)))

    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"infinitesimal part cannot be infinite"
                          (*R 0 ##Inf))))

  (testing "hyperreal?"

    (is (true? (h/hyperreal? h/zero)))

    (is (true? (h/hyperreal? h/epsilon)))

    (is (true? (h/hyperreal? h/infinity)))

    (is (true? (h/hyperreal? (*R 1 2))))

    (is (false? (h/hyperreal? 27))))

  (testing "standard"

    (is (= 0 (h/standard h/zero)))

    (is (= 1 (h/standard (*R 1 2))))

    (is (= ##Inf (h/standard h/infinity)))

    (is (= ##-Inf (h/standard h/negative-infinity))))

  (testing "="

    (is (h/= h/zero))

    (is (h/= h/zero h/zero))

    (is (h/= (*R 1 2) (*R 1 2) (*R 1 2)))

    (is (false? (h/= h/infinity (*R 0 1))))

    (is (false? (h/= h/epsilon
                     (*R 0 1)
                     (*R 0 2)
                     (*R 0 2)))))

  (testing "+"

    (is (h/= h/zero (h/+)))

    (is (h/= h/zero (h/+ h/zero)))

    (is (h/= h/zero (h/+ h/positive-infinity h/negative-infinity)))

    (is (h/= h/positive-infinity (h/+ h/positive-infinity
                                      h/positive-infinity
                                      h/epsilon)))

    (is (h/= (*R 1 9)
             (h/+ (*R 1)
                  (*R 0 2)
                  (*R 0 3)
                  (*R 0 4)))))

  (testing "-"

    (is (h/= (*R 1) (h/- (*R -1))))

    (is (h/= (*R 0 -1) (h/- h/epsilon)))

    (is (h/= h/negative-infinity
             (h/- h/positive-infinity)
             (h/- h/zero h/infinity)))

    (is (h/= (*R 1 9)
             (h/- (*R 10 10)
                  (*R 5 5)
                  h/zero
                  (*R 4 -4)))))

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

    (is (=  0 (compare h/zero     h/zero)))
    (is (= -1 (compare h/zero     h/epsilon)))
    (is (=  1 (compare h/infinity h/epsilon)))

    (let [vals-in-order [(*R -10 500)
                         (*R 0 -100)
                         (*R 0 0)
                         (*R 0 10)
                         (*R 10 -10000)
                         h/infinity]]
      (is (every? (fn [[a b c]] (h/= a b c))
                  (map vector
                       vals-in-order
                       ;; Using explicit comparator.
                       (->> (shuffle vals-in-order)
                            (apply sorted-set-by h/comparator)
                            seq)
                       ;; Using Comparable/IComparable.
                       (->> (shuffle vals-in-order)
                            (apply sorted-set)
                            seq))))))

  (testing "min"

    (is (= h/zero (h/min h/zero)))

    (is (= h/zero (h/min h/infinity h/epsilon h/zero))))

  (testing "max"

    (is (= h/zero (h/max h/zero)))

    (is (= h/infinity (h/max h/epsilon h/infinity h/zero))))

  (testing "zero?"

    (is (false? (h/zero? (*R 1))))

    (is (true? (h/zero? h/zero)))

    (is (true? (h/zero? (h/*R 0))))

    (is (true? (h/zero? (h/*R 0 0)))))

  (testing "pos?"

    (is (false? (h/pos? (*R 0 -1))))

    (is (false? (h/pos? h/zero)))

    (is (h/pos? h/epsilon))

    (is (h/pos? h/infinity))

    (is (h/pos? (*R 1 -999))))

  (testing "infinite?"

    (is (false? (h/infinite? h/zero)))

    (is (false? (h/infinite? h/epsilon)))

    (is (true? (h/infinite? h/infinity)))

    (is (true? (h/infinite? h/positive-infinity)))

    (is (true? (h/infinite? h/negative-infinity))))

  (testing "infinitesimal?"

    (is (false? (h/infinitesimal? h/zero)))

    (is (false? (h/infinitesimal? h/infinity)))

    (is (false? (h/infinitesimal? (h/*R 1 1))))

    (is (false? (h/infinitesimal? (h/*R 1 -1))))

    (is (true? (h/infinitesimal? h/epsilon)))

    (is (true? (h/infinitesimal? (h/*R 0 2))))

    (is (true? (h/infinitesimal? (h/*R 0 -2))))))
