(ns pettomato.devs.lib.number-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.number :refer [infinity]]))

(deftest infinity-tests
  (testing "Just make sure this cross platform implementation works."
    (is (< 0 infinity))))
