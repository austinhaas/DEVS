(ns pettomato.devs.mail-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.mail :as mail]))

(deftest merge-local-mail-tests
  (is (mail/local-mail=
       {:x [1 2]}
       (mail/merge-local-mail {:x [1]} {:x [2]}))))

;; Using lists, because the values are sequences, and format-mail-log is
;; specifically implemented to convert those seqs to vectors.
(def sample-mail-log [[(h/*R 0) {:a (list 1 2 3)}]
                      [(h/*R 1) {:a (list 4 5)
                                 :b (list 1 2 3)}]
                      [(h/*R 2) {:b (list 4 5 6)}]])

(deftest mail-log=-test

  (testing "Empty inputs"
    (is (true? (mail/mail-log= [] []))))

  (testing "Equal inputs"
    (is (true? (mail/mail-log= sample-mail-log
                               sample-mail-log))))

  (testing "Unequal inputs"
    (is (false? (mail/mail-log= sample-mail-log
                                (conj sample-mail-log [(h/*R 3) {:a (list 1 2 3)}])))))

  (testing "Unequal inputs; first is empty."
    (is (false? (mail/mail-log= [] sample-mail-log))))

  (testing "Unequal inputs; second is empty."
    (is (false? (mail/mail-log= sample-mail-log []))))

  (testing "Single input"
    (is (true? (mail/mail-log= sample-mail-log))))

  (testing "Four equal inputs"
    (is (true? (mail/mail-log= sample-mail-log
                               sample-mail-log
                               sample-mail-log
                               sample-mail-log))))

  (testing "Three unequal inputs"
    (is (false? (mail/mail-log= sample-mail-log
                                sample-mail-log
                                [])))))

(deftest pp-mail-log-test

  ;; This isn't a very good test.
  (testing "Sucessfully prints something, with at least as many characters as the input."
    ;; Need to explicitly call `str` on the hyperreal numbers, because cljs
    ;; doesn't let us implement a print-method for them.
    (is (< (count (str (map (fn [[t m]] [(str t) m]) sample-mail-log)))
           (count (with-out-str (mail/pp-mail-log sample-mail-log)))))))

(deftest format-mail-log-test

  (testing "Sequences are converted to vectors."
    (is (->> (mail/format-mail-log sample-mail-log)
             (mapcat (comp vals second))
             (every? vector?)))))
