(ns pettomato.devs.lib.event-log-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]
   [pettomato.devs.lib.event-log :refer [event-log= pp-event-log format-event-log]]))

;; Using lists, because the values are sequences, and format-event-log is
;; specifically implemented to convert those seqs to vectors.
(def sample-event-log [[(*R 0) {:a (list 1 2 3)}]
                       [(*R 1) {:a (list 4 5)
                                :b (list 1 2 3)}]
                       [(*R 2) {:b (list 4 5 6)}]])

(deftest event-log=-test

  (testing "Empty inputs"
    (is (true? (event-log= [] []))))

  (testing "Equal inputs"
    (is (true? (event-log= sample-event-log
                           sample-event-log))))

  (testing "Unequal inputs"
    (is (false? (event-log= sample-event-log
                            (conj sample-event-log [(*R 3) {:a (list 1 2 3)}])))))

  (testing "Unequal inputs; first is empty."
    (is (false? (event-log= [] sample-event-log))))

  (testing "Unequal inputs; second is empty."
    (is (false? (event-log= sample-event-log []))))

  (testing "Single input"
    (is (true? (event-log= sample-event-log))))

  (testing "Three equal inputs"
    (is (true? (event-log= sample-event-log
                           sample-event-log
                           sample-event-log))))

  (testing "Three unequal inputs"
    (is (false? (event-log= sample-event-log
                            sample-event-log
                            [])))))

(deftest pp-event-log-test

  ;; This isn't a very good test.
  (testing "Sucessfully prints something, with at least as many characters as the input."
    ;; Need to explicitly call `str` on the hyperreal numbers, because cljs
    ;; doesn't let us implement a print-method for them.
    (is (< (count (str (map (fn [[t m]] [(str t) m]) sample-event-log)))
           (count (with-out-str (pp-event-log sample-event-log)))))))

(deftest format-event-log-test

  (testing "Sequences are converted to vectors."
    (is (->> (format-event-log sample-event-log)
             (mapcat (comp vals second))
             (every? vector?)))))
