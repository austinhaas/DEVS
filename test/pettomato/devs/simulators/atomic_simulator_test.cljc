(ns pettomato.devs.simulators.atomic-simulator-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]))

(deftest exception-tests

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error: \(not \(= 0 10\)\)"
                        (-> (generator 10 100)
                            atomic-simulator
                            (initialize 0)
                            (collect-mail 0))))

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error: \(not \(<= 0 11 10\)\)"
                        (-> (generator 10 100)
                            atomic-simulator
                            (initialize 0)
                            (transition {:in [0]} 11))))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Illegal state for transition; sim is not imminent nor receiving mail."
                        (-> (generator 10 100)
                            atomic-simulator
                            (initialize 0)
                            (transition {} 0)))))
