(ns pettomato.devs.lib.string-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.string :as s]))

(deftest string-tests

  (is (= "xxfoo"
         (s/pad-left 5 \x "foo")))

  (is (thrown? #?(:clj AssertionError
                  :cljs js/Error)
               (s/pad-left 5 "xx" "foo")))

  (is (= "fooxx"
         (s/pad-right 5 \x "foo")))

  (is (thrown? #?(:clj AssertionError
                  :cljs js/Error)
               (s/pad-right 5 "xx" "foo")))

  (is (= "xxfooyy"
         (s/format-str "%sfoo%s" "xx" "yy"))))
