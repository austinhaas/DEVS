(ns pettomato.devs.lib.date-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.date :as date]))

(deftest date-test

  (testing "now is evaluated without error"
    (is (date/now)))

  (testing "timestamp is evaluated without error"
    (is (date/timestamp)))

  (testing "format-date is evaluated without error"
    (is (string? (date/format-date (date/now))))))
