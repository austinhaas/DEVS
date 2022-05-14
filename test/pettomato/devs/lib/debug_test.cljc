(ns pettomato.devs.lib.debug-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.debug :refer [ex-assert]]))

(deftest ex-assert-tests

  (testing "Arity 1"
    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"^Assert failed: false$"
                          (ex-assert false))))

  (testing "Arity 2"
    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"Test message."
                          (ex-assert false "Test message."))))

  (testing "Arity 3"
    (let [a 2
          b 1
          e (try (ex-assert (< a b) "Wrong." {:a a :b b})
                 (catch #?(:clj Exception :cljs :default) e e))]
      (is (= "Assert failed: Wrong.\n(< a b)"
             (ex-message e)))
      (is (= {:a a :b b}
             (ex-data e))))))
