(ns pettomato.devs.simulators.rt-atomic-simulator-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.rt-atomic-simulator :refer [rt-atomic-simulator]]))

(deftest exception-tests

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error: \(not \(= 0 10\)\)"
                        (-> (generator 10 100)
                            rt-atomic-simulator
                            (initialize 0)
                            (collect-mail 0))))

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error: \(not \(<= 0 11 10\)\)"
                        (-> (generator 10 100)
                            rt-atomic-simulator
                            (initialize 0)
                            (transition {:in [0]} 11))))

  (testing "Real-time models must handle no-op transitions."
    (is (-> (generator 10 100)
            rt-atomic-simulator
            (initialize 0)
            (transition {} 0)))))
