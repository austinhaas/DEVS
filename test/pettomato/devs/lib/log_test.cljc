(ns pettomato.devs.lib.log-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.lib.log :as log]))

(deftest standard-logging-functions

  (let [buffer      (atom [])
        buffer-push (partial swap! buffer conj)
        buffer-pop  (fn [] (ffirst (swap-vals! buffer pop)))]

    (binding [log/*handler* buffer-push
              log/*context* {:x 200}]

      (log/trace "Test message." 100)
      (log/tracef "Test message. %s" 100)
      (is (= {:message "Test message. 100" :level :trace :x 200}
             (buffer-pop)
             (buffer-pop)))

      (log/debug "Test message." 100)
      (log/debugf "Test message. %s" 100)
      (is (= {:message "Test message. 100" :level :debug :x 200}
             (buffer-pop)
             (buffer-pop)))

      (log/info "Test message." 100)
      (log/infof "Test message. %s" 100)
      (is (= {:message "Test message. 100" :level :info :x 200}
             (buffer-pop)
             (buffer-pop)))

      (log/warn "Test message." 100)
      (log/warnf "Test message. %s" 100)
      (is (= {:message "Test message. 100" :level :warn :x 200}
             (buffer-pop)
             (buffer-pop)))

      (log/error "Test message." 100)
      (log/errorf "Test message. %s" 100)
      (is (= {:message "Test message. 100" :level :error :x 200}
             (buffer-pop)
             (buffer-pop)))

      (log/fatal "Test message." 100)
      (log/fatalf "Test message. %s" 100)
      (is (= {:message "Test message. 100" :level :fatal :x 200}
             (buffer-pop)
             (buffer-pop))))))
