(ns pettomato.devs.lib.date-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.date :refer [now format-date]]))

(deftest date-test
  (testing "Just make sure they can be called without error."
    (is (string? (format-date (now))))))
