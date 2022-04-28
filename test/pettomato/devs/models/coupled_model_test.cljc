(ns pettomato.devs.models.coupled-model-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [buffer]]
   [pettomato.devs.models.coupled-model :refer [coupled-model]]))

(deftest invalid-coupled-models

  ;; Aside from the specific invalid case that is being tested, these models are
  ;; valid, but may be nonsensical.

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"A coupled model must contain at least one component model."
                        (coupled-model {} [])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"All models in routes must appear in models \(except for :network\)."
                        (coupled-model {:x (buffer 5)} [[:network :in :y :in]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"A model cannot use the same port for both input and output."
                        (coupled-model {:x (buffer 5)} [[:network :in :x :in]
                                                        [:x :in :network :out]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"A network input port cannot connect directly to a network output port"
                        (coupled-model {:x (buffer 5)} [[:network :in :network :out]]))))
