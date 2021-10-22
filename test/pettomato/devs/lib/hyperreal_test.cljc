(ns pettomato.devs.lib.hyperreal-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.hyperreal :as h :refer [H]]))

(deftest hyperreal-tests

  (is (false? (h/= h/infinity (H 0 1))))

  (is (true? (h/= h/infinity (h/+ (H 0 1) h/infinity))))

  (is (true? (h/< (H 100 0) h/infinity)))

  (is (true? (h/< (H 100 0) (H 200 -1000))))

  (is (true? (h/< (H 100 -1) (H 100 0))))

  (is (true? (h/< (H 100 -1) (H 100 0) (H 100 1) h/infinity)))

  (is (true? (h/<= (H 100 -1) (H 100 0))))

  (is (true? (h/<= (H 100 -1) (H 100 0) h/infinity h/infinity)))

  (let [vals-in-order [(H -10 500)
                       (H 0 -100)
                       (H 0 0)
                       (H 0 10)
                       (H 10 -10000)
                       h/infinity]]
    (is (every? (fn [[a b]] (h/= a b))
                (map vector
                     vals-in-order
                     (->> (shuffle vals-in-order)
                          (apply sorted-set-by h/comparator)
                          seq))))))
