(ns pettomato.devs.lib.mail-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.mail :as mail]))

(deftest merge-local-mail-tests
  (is (mail/local-mail=
       {:x [1 2]}
       (mail/merge-local-mail {:x [1]} {:x [2]}))))
