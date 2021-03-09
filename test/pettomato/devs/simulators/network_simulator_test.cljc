(ns pettomato.devs.simulators.network-simulator-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator delay1]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.network-simulator :refer [default-model->sim network-simulator]]))

(deftest exception-tests

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error: \(not \(= 0 10\)\)"
                        (let [gen (generator 10 100)
                              del (delay1 5)
                              net (network-model {:gen gen
                                                  :del del}
                                                 [[:gen :out :del :in identity]
                                                  [:del :out :network :out identity]])]
                          (-> net
                              network-simulator
                              (initialize 0)
                              (collect-mail 0)))))

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error: \(not \(<= 0 11 10\)\)"
                        (let [gen (generator 10 100)
                              del (delay1 5)
                              net (network-model {:gen gen
                                                  :del del}
                                                 [[:gen :out :del :in identity]
                                                  [:del :out :network :out identity]])]
                          (-> net
                              network-simulator
                              (initialize 0)
                              (transition {:in [0]} 11)))))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Unknown model type."
                        (default-model->sim nil))))
