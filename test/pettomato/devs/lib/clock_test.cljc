(ns pettomato.devs.lib.clock-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.clock :as clock]
   [pettomato.devs.lib.hyperreal :as h]))

(deftest clock-tests

  (testing "initial sim time"
    (is (h/= h/zero
             (-> (clock/clock 1000 h/zero)
                 clock/get-sim-time))))

  (testing "Advancing 1 second"
    (is (h/= (h/*R 1000)
             (-> (clock/clock 0 h/zero)
                 (clock/set-wall-time 1000)
                 clock/get-sim-time)
             (-> (clock/clock 0 h/zero)
                 (clock/get-sim-time 1000)))))

  (testing "wall-time must be nondecreasing"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (-> (clock/clock 0 h/zero)
                     (clock/set-wall-time 1000)
                     (clock/set-wall-time 900))))))

(deftest scale-factor-tests

  (testing "default is 1.0"
    (is (= 1.0 (-> (clock/clock 0 h/zero)
                   clock/get-scale-factor))))

  (testing "setting initially to 2.0"
    (is (= 2.0 (-> (clock/clock 0 h/zero :scale-factor 2.0)
                   clock/get-scale-factor))))

  (testing "setting initially to 1.0, then changing to 2.0"
    (is (= 2.0 (-> (clock/clock 0 h/zero :scale-factor 1.0)
                   (clock/set-scale-factor 2.0)
                   clock/get-scale-factor))))

  (testing "sim advances slower than wall-time"
    (is (h/= (h/*R 500)
             (-> (clock/clock 0 h/zero :scale-factor 0.5)
                 (clock/set-wall-time 1000)
                 clock/get-sim-time))))

  (testing "sim advances faster than wall-time"
    (is (h/= (h/*R 2000)
             (-> (clock/clock 0 h/zero :scale-factor 2.0)
                 (clock/set-wall-time 1000)
                 clock/get-sim-time))))

  (testing "sim advances in reverse"
    (is (h/= (h/*R -1000)
             (-> (clock/clock 0 h/zero :scale-factor -1.0)
                 (clock/set-wall-time 1000)
                 clock/get-sim-time))))

  (testing "Changing scale-factor while running"
    (is (h/= (h/*R 1500)
             (-> (clock/clock 0 h/zero :scale-factor 1.0)
                 (clock/set-wall-time 1000)
                 (clock/set-scale-factor 0.5) ; arity 1
                 (clock/set-wall-time 2000)
                 clock/get-sim-time)
             (-> (clock/clock 0 h/zero :scale-factor 1.0)
                 (clock/set-scale-factor 1000 0.5) ; arity 2
                 (clock/set-wall-time 2000)
                 clock/get-sim-time))))

  (testing "pause the clock"
    (is (h/= (h/*R 2000)
             (-> (clock/clock 0 h/zero)
                 (clock/set-wall-time 2000)
                 (clock/set-scale-factor 0) ; arity 1
                 (clock/set-wall-time 3000)
                 clock/get-sim-time)
             (-> (clock/clock 0 h/zero)
                 (clock/set-wall-time 1000)
                 (clock/set-scale-factor 2000 0) ; arity 2
                 (clock/set-wall-time 3000)
                 clock/get-sim-time))))

  (testing "pausing, then unpause the clock"
    (is (h/= (h/*R 3000)
             (-> (clock/clock 0 h/zero)
                 (clock/set-wall-time 1000)
                 (clock/set-scale-factor 2000 0)
                 (clock/set-wall-time 3000)
                 (clock/set-scale-factor 4000 1.0)
                 (clock/set-wall-time 5000)
                 clock/get-sim-time)))))
