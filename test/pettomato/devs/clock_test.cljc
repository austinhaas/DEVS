(ns pettomato.devs.clock-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.clock :as clock]
   [pettomato.devs.lib.hyperreal :as h]))

;; Some of these tests are problematic in CLJS, because we can't
;; simply call Thread/sleep to wait for wall-time to advance. For most
;; tests, we use an artificial source for wall-time, which simplifies
;; the tests and allows us to test for exact results.

(defn make-artificial-wall-time-fn []
  (let [wall-time (atom 0)]
    (fn [] (swap! wall-time + 1000))))

(defn make-bad-artificial-wall-time-fn []
  (let [wall-time (atom 0)]
    (fn [] (swap! wall-time - 1000))))

(deftest basic-functionality

  (testing "Defaults."
    #?(:clj
       (is (h/<= (h/*R 10)
                 (let [c (clock/clock)]
                   (Thread/sleep 10)
                   (clock/get-sim-time c))
                 (h/*R 12)))
       :cljs
       (is (h/<= (h/*R 0)
                 (clock/get-sim-time (clock/clock))
                 (h/*R 1)))))

  (testing "Advancing 1 second"
    (is (h/= (h/*R 1000)
             (-> (clock/clock :wall-time-fn (make-artificial-wall-time-fn))
                 clock/get-sim-time))))

  (testing "Advancing 2 seconds"
    (is (h/= (h/*R 2000)
             (let [c (clock/clock :wall-time-fn (make-artificial-wall-time-fn))]
               (clock/get-sim-time c)
               (clock/get-sim-time c))))))

(deftest sim-time-tests

  (testing "sim-time must be a hyperreal."
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (clock/clock :sim-time 0))))

  (testing "Supplied sim-time."
    #?(:clj
       (is (h/<= (h/*R 110)
                 (let [c (clock/clock :sim-time (h/*R 100))]
                   (Thread/sleep 10)
                   (clock/get-sim-time c))
                 (h/*R 112)))
       :cljs
       (is (h/<= (h/*R 100)
                 (clock/get-sim-time (clock/clock :sim-time (h/*R 100)))
                 (h/*R 101))))))

(deftest wall-time-fn-tests

  (testing "User-supplied wall-time-fn."
    (is (h/= (h/*R 1000)
             (-> (clock/clock :wall-time-fn (make-artificial-wall-time-fn))
                 clock/get-sim-time))))

  (testing "wall-time-fn must be a function"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (clock/clock :wall-time-fn 1000))))

  (testing "Wall-time must be nondecreasing."
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (-> (clock/clock :wall-time-fn (make-bad-artificial-wall-time-fn))
                     clock/get-sim-time)))))

(deftest paused-tests

  (testing "paused? must be a boolean."
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (clock/clock :paused? (constantly false)))))

  (testing "Unpaused by default."
    (let [c (clock/clock)]
      (is (false? (clock/paused? c)))))

  (testing "Pause, then unpause"
    (let [c (clock/clock :paused? true :wall-time-fn (make-artificial-wall-time-fn))]
      (is (true? (clock/paused? c)))
      (is (h/= (h/*R 0) (clock/get-sim-time c)))
      (is (h/= (h/*R 0) (clock/get-sim-time c)))
      (is (h/= (h/*R 0) (clock/get-sim-time c)))
      (let [c (clock/unpause c)]
        (is (false? (clock/paused? c)))
        (is (h/= (h/*R 1000) (clock/get-sim-time c)))
        (is (h/= (h/*R 2000) (clock/get-sim-time c)))))))

(deftest scale-factor-tests

  (testing "scale-factor must be a number."
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (clock/clock :scale-factor (constantly 1)))))

  (testing "Default is 1.0."
    (is (= 1.0 (-> (clock/clock)
                   clock/get-scale-factor))))

  (testing "Setting initially to 2.0."
    (is (= 2.0 (-> (clock/clock :scale-factor 2.0)
                   clock/get-scale-factor))))

  (testing "Setting initially to 1.0, then changing to 2.0."
    (is (= 2.0 (-> (clock/clock :scale-factor 1.0)
                   (clock/set-scale-factor 2.0)
                   clock/get-scale-factor))))

  (testing "Clock advances slower than wall-time."
    (is (h/= (h/*R 500)
             (-> (clock/clock :scale-factor 0.5 :wall-time-fn (make-artificial-wall-time-fn))
                 clock/get-sim-time))))

  (testing "Clock advances faster than wall-time."
    (is (h/= (h/*R 2000)
             (-> (clock/clock :scale-factor 2.0 :wall-time-fn (make-artificial-wall-time-fn))
                 clock/get-sim-time))))

  (testing "Clock advances in reverse."
    (is (h/= (h/*R -1000)
             (-> (clock/clock :scale-factor -1.0 :wall-time-fn (make-artificial-wall-time-fn))
                 clock/get-sim-time))))

  (testing "Changing scale-factor while running."
    (let [c (clock/clock :scale-factor 1.0 :wall-time-fn (make-artificial-wall-time-fn))]
      (is (h/= (h/*R 1000) (clock/get-sim-time c)))
      (let [c (clock/set-scale-factor c 0.5)]
        (is (h/= (h/*R 2500) (clock/get-sim-time c))))))

  (testing "Stop the clock by setting scale-factor to zero."
    (let [c (clock/clock :scale-factor 1.0 :wall-time-fn (make-artificial-wall-time-fn))]
      (is (h/= (h/*R 1000) (clock/get-sim-time c)))
      (let [c (clock/set-scale-factor c 0)]
        (is (h/= (h/*R 2000) (clock/get-sim-time c))))))

  (testing "Stop and restart the clock by setting scale-factor."
    (let [c (clock/clock :scale-factor 1.0 :wall-time-fn (make-artificial-wall-time-fn))]
      (is (h/= (h/*R 1000) (clock/get-sim-time c)))
      (let [c (clock/set-scale-factor c 0)]
        (is (h/= (h/*R 2000) (clock/get-sim-time c)))
        (is (h/= (h/*R 2000) (clock/get-sim-time c)))
        (is (h/= (h/*R 2000) (clock/get-sim-time c)))
        (let [c (clock/set-scale-factor c 1.0)]
          (is (h/= (h/*R 3000) (clock/get-sim-time c))))))))

(deftest pause-and-scale-factor-tests

  (testing "Pausing and scale-factor changes can be interleaved."
    (let [c (clock/clock :wall-time-fn (make-artificial-wall-time-fn))]
      (is (h/= (h/*R 1000) (clock/get-sim-time c)))                ;; wall-time = 1000
      (let [c (clock/set-scale-factor c 0.5)]                      ;; wall-time = 2000
        (is (h/= (h/*R 2500) (clock/get-sim-time c)))              ;; wall-time = 3000
        (let [c (clock/pause c)]                                   ;; wall-time = 4000
          (is (h/= (h/*R 3000) (clock/get-sim-time c)))            ;; wall-time = 5000
          (is (h/= (h/*R 3000) (clock/get-sim-time c)))            ;; wall-time = 6000
          (let [c (clock/set-scale-factor c 2.0)]                  ;; wall-time = 7000
            (is (h/= (h/*R 3000) (clock/get-sim-time c)))          ;; wall-time = 8000
            (let [c (clock/unpause c)]                             ;; wall-time = 9000
              (is (h/= (h/*R 5000) (clock/get-sim-time c))))))))))
