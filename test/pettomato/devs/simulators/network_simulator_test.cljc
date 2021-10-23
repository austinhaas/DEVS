(ns pettomato.devs.simulators.network-simulator-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [generator fixed-delay]]
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.simulators.network-simulator :refer [default-find-simulator network-simulator]]))

(deftest exception-tests

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error"
                        (let [gen (generator (*R 10) 100)
                              del (fixed-delay (*R 5))
                              net (network-model {:gen gen
                                                  :del del}
                                                 [[:gen :out :del :in identity]
                                                  [:del :out :network :out identity]])]
                          (-> net
                              network-simulator
                              (initialize h/zero)
                              (collect-mail h/zero)))))

  (is (thrown-with-msg? #?(:clj AssertionError
                           :cljs js/Error)
                        #"synchronization error"
                        (let [gen (generator (*R 10) 100)
                              del (fixed-delay (*R 5))
                              net (network-model {:gen gen
                                                  :del del}
                                                 [[:gen :out :del :in identity]
                                                  [:del :out :network :out identity]])]
                          (-> net
                              network-simulator
                              (initialize h/zero)
                              (transition {:in [0]} (*R 11))))))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Unknown model type."
                        (default-find-simulator nil nil))))
