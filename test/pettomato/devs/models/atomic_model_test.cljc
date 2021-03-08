(ns pettomato.devs.models.atomic-model-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.models.atomic-model :refer [atomic-model]]))

(deftest invalid-atomic-models

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #":initial-elapsed-time must be a number; value: "
                        (atomic-model
                         :initial-elapsed-time nil)))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #":internal-update must implement IFn; value: 100"
                        (atomic-model
                         :internal-update 100)))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #":external-update must implement IFn; value: 100"
                        (atomic-model
                         :external-update 100)))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #":confluent-update must implement IFn; value: 100"
                        (atomic-model
                         :confluent-update 100)))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #":output must implement IFn; value: 100"
                        (atomic-model
                         :output 100)))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #":time-advance must implement IFn; value: 100"
                        (atomic-model
                         :time-advance 100)))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Invalid options supplied to atomic-model: \{:foo 100\}"
                        (atomic-model
                         :foo 100))))
