(ns pettomato.devs.models.network-model-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :refer [buffer simple-executive]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.models.network-model :refer [network-model]]))

(deftest invalid-network-models

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Executive must be an executive model."
                        (network-model :exec
                                       [(buffer (h/*R 5)) h/zero])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"All models in routes must appear in models \(except for :network\)."
                        (network-model :exec
                                       [(simple-executive) h/zero]
                                       {:x (buffer (h/*R 5))}
                                       [[:network :in :y :in]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"A model cannot use the same port for both input and output."
                        (network-model :exec
                                       [(simple-executive) h/zero]
                                       {:x (buffer (h/*R 5))}
                                       [[:network :in :x :in]
                                        [:x :in :network :out]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"A network input port cannot connect directly to a network output port"
                        (network-model :exec
                                       [(simple-executive) h/zero]
                                       {:x (buffer (h/*R 5))}
                                       [[:network :in :network :out]]))))
