(ns pettomato.devs.models.network-structure-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.models.network-structure :refer [mail-equal? route-messages]]))

(deftest route-messages-test
  (is (mail-equal? {:b {:in [2 3 4]}
                    :c {:in [1 2 3]}}
                   (route-messages
                    {:a {:out {:b {:in inc}
                               :c {:in identity}}}}
                    {:a {:out [1 2 3]}}))))
