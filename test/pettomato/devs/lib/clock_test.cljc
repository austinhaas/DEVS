(ns pettomato.devs.lib.clock-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.clock :as clock]))

(deftest clock-tests

  (testing "initial sim time"
    (is (= 0 (-> (clock/clock 1000 0)
                 clock/get-sim-time))))

  (testing "Advancing 1 second"
    (is (= 1000 (-> (clock/clock 0 0)
                    (clock/advance 1000)
                    clock/get-sim-time))))

  (testing "wall-time must be nondecreasing"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (-> (clock/clock 0 0)
                     (clock/advance 1000)
                     (clock/advance 900))))))

(deftest scale-factor-tests

  (testing "default is 1.0"
    (is (= 1.0 (-> (clock/clock 0 0)
                   clock/get-scale-factor))))

  (testing "setting initially to 2.0"
    (is (= 2.0 (-> (clock/clock 0 0 :scale-factor 2.0)
                   clock/get-scale-factor))))

  (testing "setting initially to 1.0, then changing to 2.0"
    (is (= 2.0 (-> (clock/clock 0 0 :scale-factor 1.0)
                   (clock/set-scale-factor 0 2.0)
                   clock/get-scale-factor))))

  (testing "sim advances slower than wall-time"
    (is (= 500 (-> (clock/clock 0 0 :scale-factor 0.5)
                   (clock/advance 1000)
                   clock/get-sim-time))))

  (testing "sim advances faster than wall-time"
    (is (= 2000 (-> (clock/clock 0 0 :scale-factor 2.0)
                    (clock/advance 1000)
                    clock/get-sim-time))))

  (testing "sim advances in reverse"
    (is (= -1000 (-> (clock/clock 0 0 :scale-factor -1.0)
                     (clock/advance 1000)
                     clock/get-sim-time))))

  (testing "Changing scale-factor while running"
    (is (= 1500 (-> (clock/clock 0 0 :scale-factor 1.0)
                    (clock/set-scale-factor 1000 0.5)
                    (clock/advance 2000)
                    clock/get-sim-time))))

  (testing "pause the clock"
    (is (= 2000 (-> (clock/clock 0 0)
                    (clock/advance 1000)
                    (clock/set-scale-factor 2000 0)
                    (clock/advance 3000)
                    clock/get-sim-time))))

  (testing "pausing, then unpause the clock"
    (is (= 3000 (-> (clock/clock 0 0)
                    (clock/advance 1000)
                    (clock/set-scale-factor 2000 0)
                    (clock/advance 3000)
                    (clock/set-scale-factor 4000 1.0)
                    (clock/advance 5000)
                    clock/get-sim-time)))))
