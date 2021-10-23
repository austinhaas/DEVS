(ns pettomato.devs.lib.hyperreal-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]))

;; TODO: Add a lot more tests.

(deftest hyperreal-tests

  (is (false? (h/= h/infinity (*R 0 1))))

  (is (true? (h/< h/infinity (h/+ (*R 0 1) h/infinity))))

  (is (true? (h/< (*R 100 0) h/infinity)))

  (is (true? (h/< (*R 100 0) (*R 200 -1000))))

  (is (true? (h/< (*R 100 -1) (*R 100 0))))

  (is (true? (h/< (*R 100 -1) (*R 100 0) (*R 100 1) h/infinity)))

  (is (true? (h/<= (*R 100 -1) (*R 100 0))))

  (is (true? (h/<= (*R 100 -1) (*R 100 0) h/infinity h/infinity)))

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
