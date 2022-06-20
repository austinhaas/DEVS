(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing async]])
   #?(:cljs [cljs.core.async :as async :refer-macros [go]])
   [pettomato.devs :as devs]
   [pettomato.devs.examples :as ex]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.mail :refer [mail-log=]]))

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (is (mail-log=
         [[(h/*R 2 0)  {:out ["x"]}]
          [(h/*R 4 0)  {:out ["x"]}]
          [(h/*R 6 0)  {:out ["x"]}]
          [(h/*R 8 0)  {:out ["x"]}]
          [(h/*R 10 0) {:out ["x"]}]]
         (-> (ex/generator (repeat [(h/*R 2 0) ["x"]]))
             devs/atomic-simulator
             (devs/afap-root-coordinator :end (h/*R 10 0))))))

  (testing "Specifying a non-zero start time."
    (is (mail-log=
         [[(h/*R 7 0) {:out ["x"]}]
          [(h/*R 9 0) {:out ["x"]}]]
         (-> (ex/generator (repeat [(h/*R 2 0) ["x"]]))
             devs/atomic-simulator
             (devs/afap-root-coordinator :start (h/*R 5 0) :end (h/*R 10 0))))))

  (testing "A simple network."
    (is (mail-log=
         [[(h/*R 15 0) {:out ["x"]}]
          [(h/*R 25 0) {:out ["y"]}]]
         (let [gen (ex/generator [[(h/*R 10) ["x"]]
                                  [(h/*R 10) ["y"]]])
               buf (ex/buffer (h/*R 5))
               net (devs/static-network-model
                    {:gen [gen h/zero]
                     :buf [buf h/zero]}
                    [[:gen :out :buf :in]
                     [:buf :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator))))))

(deftest initial-elapsed-tests

  (testing "An atomic simulation."
    (is (mail-log=
         [[(h/*R 5)  {:out ["x"]}]
          [(h/*R 15) {:out ["y"]}]]
         (let [gen (ex/generator [[(h/*R 10) ["x"]]
                                  [(h/*R 10) ["y"]]])]
           (-> (devs/atomic-simulator gen :elapsed (h/*R 5))
               devs/afap-root-coordinator)))))

  (testing "Atomic: initial-elapsed must be >= 0."
    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"initial-elapsed must be <= 0"
                          (-> (ex/generator (repeat [(h/*R 2) ["x"]]))
                              (devs/atomic-simulator :elapsed (h/*R -1))
                              (devs/afap-root-coordinator :end (h/*R 10))))))

  (testing "Atomic: tn can't be before start time. "
    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"NIA violation"
                          (-> (ex/generator (repeat [(h/*R 2) ["x"]]))
                              (devs/atomic-simulator :elapsed (h/*R 3))
                              (devs/afap-root-coordinator :end (h/*R 10))))))

  (testing "Atomic: tn can't be equal to start time. "
    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"NIA violation"
                          (-> (ex/generator (repeat [(h/*R 2) ["x"]]))
                              (devs/atomic-simulator :elapsed (h/*R 2))
                              (devs/afap-root-coordinator :end (h/*R 10))))))

  (testing "A network simulation."
    (is (mail-log=
         [[(h/*R 13 0) {:out ["x"]}]
          [(h/*R 23 0) {:out ["y"]}]]
         (let [gen (ex/generator [[(h/*R 10) ["x"]]
                                  [(h/*R 10) ["y"]]])
               buf (ex/buffer (h/*R 5))
               net (devs/static-network-model
                    {:gen [gen h/zero]
                     :buf [buf h/zero]}
                    [[:gen :out :buf :in]
                     [:buf :out :network :out]])]
           (-> (devs/network-simulator net :elapsed (h/*R 2))
               devs/afap-root-coordinator)))))

  (testing "Network: tn can't be before start time."
    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"tn can't be in the past.*"
                          (let [gen (ex/generator [[(h/*R 10) ["x"]]])
                                net (devs/static-network-model
                                     {:gen [gen h/zero]}
                                     [[:gen :out :network :out]])]
                            (-> (devs/network-simulator net :elapsed (h/*R 20))
                                devs/afap-root-coordinator)))))

  (testing "Network: tn can't be equal start time."
    (is (thrown-with-msg? #?(:clj  clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo)
                          #"tn can't be in the past.*"
                          (let [gen (ex/generator [[(h/*R 10) ["x"]]])
                                net (devs/static-network-model
                                     {:gen [gen h/zero]}
                                     [[:gen :out :network :out]])]
                            (-> (devs/network-simulator net :elapsed (h/*R 10))
                                devs/afap-root-coordinator)))))

  (testing "An atomic model within a network simulation."
    (is (mail-log=
         [[(h/*R 13 0) {:out ["x"]}]
          [(h/*R 23 0) {:out ["y"]}]]
         (let [gen (ex/generator [[(h/*R 10) ["x"]]
                                  [(h/*R 10) ["y"]]])
               buf (ex/buffer (h/*R 5))
               net (devs/static-network-model
                    {:gen [gen (h/*R 2)]
                     :buf [buf h/zero]}
                    [[:gen :out :buf :in]
                     [:buf :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator))))))

(deftest elapsed-time-tests

  (testing "The model can keep track of total elapsed time."
    (is (mail-log=
         [[(h/*R 15) {:out [1 2 3]}]]
         (let [gen (ex/generator [[(h/*R 5) [[(h/*R 10) 1]]]
                                  [(h/*R 1) [[(h/*R 9)  2]]]
                                  [(h/*R 3) [[(h/*R 6)  3]]]])
               buf (ex/buffer+)
               net (devs/static-network-model
                    {:gen [gen h/zero]
                     :buf [buf h/zero]}
                    [[:gen :out :buf :in]
                     [:buf :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator))))))

(deftest route-function-tests

  (testing "Using route functions"
    (is (mail-log=
         [[(h/*R 15 0) {:out [["x"]]}]
          [(h/*R 25 0) {:out [["y"]]}]]
         (let [gen (ex/generator [[(h/*R 10) ["x"]]
                                  [(h/*R 10) ["y"]]])
               buf (ex/buffer+)
               net (devs/static-network-model
                    {:gen [gen h/zero]
                     :buf [buf h/zero]}
                    [[:gen :out :buf :in (map (fn [x] [(h/*R 5) x]))]
                     [:buf :out :network :out (map vector)]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator))))))

(deftest confluence-tests

  (testing "Internal before external."
    (is (mail-log=
         [[(h/*R 20 0) {:out ["x"]}]
          [(h/*R 30 0) {:out ["y"]}]]
         (let [gen (ex/generator [[(h/*R 10) ["x"]]
                                  [(h/*R 10) ["y"]]])
               buf (ex/buffer (h/*R 10))
               net (devs/static-network-model
                    {:gen [gen h/zero]
                     :buf [buf h/zero]}
                    [[:gen :out :buf :in]
                     [:buf :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator)))))

  (testing "External before internal."
    (is (mail-log=
         [[(h/*R 20 0) {:out ["x"]}]]
         (let [gen (ex/generator [[(h/*R 10) ["x"]]
                                  [(h/*R 10) ["y"]]])
               buf (ex/buffer2 (h/*R 10))
               net (devs/static-network-model
                    {:gen [gen h/zero]
                     :buf [buf h/zero]}
                    [[:gen :out :buf :in]
                     [:buf :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator))))))

(deftest deeply-nested-structure

  (testing "A trivial deeply nested structure."
    (is (mail-log=
         [[(h/*R 5) {:out [[:buf-1 [:buf-2 [:buf-3 0]]]]}]
          [(h/*R 9) {:out [[:buf-1 [:buf-2 [:buf-3 2]]]]}]]
         (let [f   (fn [buf i]
                     (let [id (keyword (str "buf-" i))]
                       (devs/static-network-model
                        {id [buf h/zero]}
                        [[:network :in id :in (map (fn [x] [id x]))]
                         [id :out :network :out]])))
               gen (ex/generator (for [i (range)] [(h/*R 2) [i]]))
               buf (-> (ex/buffer (h/*R 3)) (f 1) (f 2) (f 3))
               net (devs/static-network-model
                    {:gen   [gen h/zero]
                     :buf-0 [buf h/zero]}
                    [[:gen :out :buf-0 :in]
                     [:buf-0 :out :network :out]])]
           (-> (devs/network-simulator net)
               (devs/afap-root-coordinator :end (h/*R 10))))))))

(deftest dynamic-network-tests

  (testing "Dynamic behavior."
    (is (mail-log=
         [[(h/*R 7 1) {:out [0]}]
          [(h/*R 9 1) {:out [1]}]]
         (let [gen (ex/generator
                    [[(h/*R 5) [[:add-model :gen-1 [(ex/generator
                                                     (for [i (range)] [(h/*R 2) [i]]))
                                                    h/zero]]
                                [:connect [:gen-1 :out :network :out]]]]
                     [(h/*R 5) [[:disconnect [:gen-1 :out :network :out]]
                                [:rem-model :gen-1]]]])
               net (devs/simple-network-model
                    :exec
                    {:gen [gen h/zero]}
                    [[:gen :out :exec :in]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator)))))

  (testing "Dynamic behavior; order doesn't matter."
    (is (mail-log=
         [[(h/*R 7 1) {:out [0]}]
          [(h/*R 9 1) {:out [1]}]]
         (let [gen (ex/generator [[(h/*R 5) [[:connect [:gen-1 :out :network :out]]
                                             [:add-model :gen-1 [(ex/generator
                                                                  (for [i (range)] [(h/*R 2) [i]]))
                                                                 h/zero]]]]
                                  [(h/*R 5) [[:rem-model :gen-1]
                                             [:disconnect [:gen-1 :out :network :out]]]]])
               net (devs/simple-network-model
                    :exec
                    {:gen [gen h/zero]}
                    [[:gen :out :exec :in]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator)))))

  (testing "Remove an atomic model just before it can emit a message."
    ;; In order to remove :gen before it can send its next message,
    ;; :sc-gen must emit its disconnect message two epsilon
    ;; prior. This is because :exec will take one epsilon to change
    ;; its state, and :gen must be removed at least one epsilon before
    ;; it is scheduled to emit its message.
    (is (mail-log=
         []
         (let [sc-gen (ex/generator [[(h/*R 10 -2) [[:disconnect [:gen :out :network :out]]
                                                    [:rem-model :gen]]]])
               net    (devs/simple-network-model
                       :exec
                       {:sc-gen [sc-gen h/zero]
                        :gen    [(ex/generator [[(h/*R 10) ["x"]]])
                                 h/zero]}
                       [[:sc-gen :out :exec :in]
                        [:gen :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator)))))

  (testing "Remove an atomic model when it is imminent."
    ;; The message is still sent, because outgoing mail is collected
    ;; and delivered before structure changes are applied.
    (is (mail-log=
         [[(h/*R 10) {:out ["x"]}]]
         (let [sc-gen (ex/generator [[(h/*R 10 -1) [[:disconnect [:gen :out :network :out]]
                                                    [:rem-model :gen]]]])
               net    (devs/simple-network-model
                       :exec
                       {:sc-gen [sc-gen h/zero]
                        :gen    [(ex/generator [[(h/*R 10) ["x"]]])
                                 h/zero]}
                       [[:sc-gen :out :exec :in]
                        [:gen :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator)))))

  (testing "Remove an atomic model after it is imminent."
    (is (mail-log=
         [[(h/*R 10) {:out ["x"]}]]
         (let [sc-gen (ex/generator [[(h/*R 10 -1) [[:disconnect [:gen :out :network :out]]
                                                    [:rem-model :gen]]]])
               net    (devs/simple-network-model
                       :exec
                       {:sc-gen [sc-gen h/zero]
                        :gen    [(ex/generator [[(h/*R 10) ["x"]]])
                                 h/zero]}
                       [[:sc-gen :out :exec :in]
                        [:gen :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator)))))

  (testing "Remove a message recipient before it is imminent."
    ;; In this case, :buf will receive the message and process it, but
    ;; it will be removed immediately after, when the structure
    ;; changes are processed.
    (is (mail-log=
         []
         (let [sc-gen (ex/generator [[(h/*R 10 -1) [[:disconnect [:buf :out :network :out]]
                                                    [:disconnect [:gen :out :buf :in]]
                                                    [:rem-model :buf]]]])
               net    (devs/simple-network-model
                       :exec
                       {:sc-gen [sc-gen h/zero]
                        :gen    [(ex/generator [[(h/*R 10) ["x"]]])
                                 h/zero]
                        :buf    [(ex/buffer (h/*R 5))
                                 h/zero]}
                       [[:sc-gen :out :exec :in]
                        [:gen :out :buf :in]
                        [:buf :out :network :out]])]
           (-> (devs/network-simulator net)
               devs/afap-root-coordinator)))))

  (testing "Moving a sender from one network to another. #1"
    (is (mail-log=
         [[(h/*R 3 1)  {:out-1 ["x"]}]
          [(h/*R 4 1)  {:out-1 ["x"]}]
          [(h/*R 5 1)  {:out-1 ["x"]}]
          [(h/*R 6 1)  {:out-1 ["x"]}]
          [(h/*R 7 1)  {:out-1 ["x"]}]
          [(h/*R 8 1)  {:out-2 ["x"]}]
          [(h/*R 9 1)  {:out-2 ["x"]}]]
         (let [net-1 (devs/simple-network-model
                      :exec
                      {:sc-gen [(ex/generator [[(h/*R 2) [[:add-model :gen [(ex/generator (repeat 10 [(h/*R 1) ["x"]]))
                                                                            h/zero]]
                                                          [:connect [:gen :out :network :out]]]]
                                               [(h/*R 5) [[:rem-model :gen]
                                                          [:disconnect [:gen :out :network :out]]]]])
                                h/zero]}
                      [[:sc-gen :out :exec :in]])
               net-2 (devs/simple-network-model
                      :exec
                      {:sc-gen [(ex/generator [[(h/*R 7) [[:add-model :gen [(ex/generator (repeat 10 [(h/*R 1) ["x"]]))
                                                                            h/zero]]
                                                          [:connect [:gen :out :network :out]]]]])
                                h/zero]}
                      [[:sc-gen :out :exec :in]])
               net   (devs/static-network-model
                      {:net-1 [net-1 h/zero]
                       :net-2 [net-2 h/zero]}
                      [[:net-1 :out :network :out-1]
                       [:net-2 :out :network :out-2]])]
           (-> (devs/network-simulator net)
               (devs/afap-root-coordinator :end (h/*R 10)))))))

  (testing "Moving a sender from one network to another. #2"
    ;; This works because the second generator is started with an
    ;; elapsed time that matches the time when it was removed from the
    ;; first network.
    (is (mail-log=
         [[(h/*R 7 1)  {:out-1 ["x"]}]
          [(h/*R 9 1)  {:out-1 ["x"]}]
          [(h/*R 11 1) {:out-2 ["x"]}]
          [(h/*R 13 1) {:out-2 ["x"]}]]
         (let [net-1 (devs/simple-network-model
                      :exec
                      {:sc-gen [(ex/generator [[(h/*R 5) [[:add-model :gen [(ex/generator (repeat 10 [(h/*R 2) ["x"]]))
                                                                            h/zero]]
                                                          [:connect [:gen :out :network :out]]]]
                                               [(h/*R 5) [[:rem-model :gen]
                                                          [:disconnect [:gen :out :network :out]]]]])
                                h/zero]}
                      [[:sc-gen :out :exec :in]])
               net-2 (devs/simple-network-model
                      :exec
                      {:sc-gen [(ex/generator [[(h/*R 10) [[:add-model :gen [(ex/generator (repeat 10 [(h/*R 2) ["x"]]))
                                                                             (h/*R 1)]] ; important
                                                           [:connect [:gen :out :network :out]]]]])
                                h/zero]}
                      [[:sc-gen :out :exec :in]])
               net   (devs/static-network-model
                      {:net-1 [net-1 h/zero]
                       :net-2 [net-2 h/zero]}
                      [[:net-1 :out :network :out-1]
                       [:net-2 :out :network :out-2]])]
           (-> (devs/network-simulator net)
               (devs/afap-root-coordinator :end (h/*R 15)))))))

  (testing "Move a receiver from one network to another."
    ;; This is a bit of a hack. Currently, we don't have a way to
    ;; preserve the current state of a buffer that we want to move,
    ;; which means that a buffered message will get dropped when we
    ;; remove the old buffer and add the new buffer (because it is
    ;; completely new).

    ;; So, instead, to test that the model won't miss an incoming
    ;; message, we use a route transducer to record incoming messages
    ;; in an atom, and then check the atom at the end of the test.
    (let [a1    (atom [])
          a2    (atom [])
          tx1   (map (fn [x] (swap! a1 conj x) x))
          tx2   (map (fn [x] (swap! a2 conj x) x))
          net-1 (devs/simple-network-model
                 :exec
                 {:sc-gen [(ex/generator [[(h/*R 5 -2) [[:rem-model :buf]
                                                        [:disconnect [:network :in :buf :in tx1]]
                                                        [:disconnect [:buf :out :network :out]]]]])
                           h/zero]
                  :buf    [(ex/buffer (h/*R 1))
                           h/zero]}
                 [[:sc-gen :out :exec :in]
                  [:network :in :buf :in tx1]
                  [:buf :out :network :out]])
          net-2 (devs/simple-network-model
                 :exec
                 {:sc-gen [(ex/generator [[(h/*R 5 -2) [[:add-model :buf [(ex/buffer (h/*R 1))
                                                                          h/zero]]
                                                        [:connect [:network :in :buf :in tx2]]
                                                        [:connect [:buf :out :network :out]]]]])
                           h/zero]}
                 [[:sc-gen :out :exec :in]])
          net   (devs/static-network-model
                 {:gen   [(ex/generator (for [i (range 10)] [(h/*R 2) [(str "msg-" i)]]))
                          h/zero]
                  :net-1 [net-1 h/zero]
                  :net-2 [net-2 h/zero]}
                 [[:gen :out :net-1 :in]
                  [:gen :out :net-2 :in]
                  [:net-1 :out :network :out-1]
                  [:net-2 :out :network :out-2]])]
      (is (mail-log=
           [[(h/*R 3) {:out-1 ["msg-0"]}]
            ;; msg-1 is lost, because we didn't preserve the buffer's
            ;; state when we moved it.
            [(h/*R 7) {:out-2 ["msg-2"]}]
            [(h/*R 9) {:out-2 ["msg-3"]}]]
           (-> (devs/network-simulator net)
               (devs/afap-root-coordinator :end (h/*R 10))
               doall)))
      (is (= [["msg-0" "msg-1"] ["msg-2" "msg-3" "msg-4"]]
             [@a1 @a2]))))

  (testing "Add and remove a NETWORK model."
    (is (mail-log=
         [[(h/*R 13) {:out [5]}]
          [(h/*R 15) {:out [6]}]
          [(h/*R 17) {:out [7]}]
          [(h/*R 19) {:out [8]}]]
         (let [sc-gen (ex/generator
                       [[(h/*R 10) [[:add-model :net-1 [(devs/simple-network-model
                                                         :exec
                                                         {:buf [(ex/buffer (h/*R 1)) h/zero]}
                                                         [[:network :in :buf :in]
                                                          [:buf :out :network :out]])
                                                        h/zero]]
                                    [:connect [:gen :out :net-1 :in]]
                                    [:connect [:net-1 :out :network :out]]]]
                        [(h/*R 10) [[:rem-model :net-1]
                                    [:disconnect [:gen :out :net-1 :in]]
                                    [:disconnect [:net-1 :out :network :out]]]]])
               gen    (ex/generator (for [i (range)] [(h/*R 2) [i]]))
               net    (devs/simple-network-model
                       :exec
                       {:sc-gen [sc-gen h/zero]
                        :gen    [gen    h/zero]}
                       [[:sc-gen :out :exec :in]])]
           (-> (devs/network-simulator net)
               (devs/afap-root-coordinator :end (h/*R 30)))))))

  (testing "ad-hoc network structure change test"
    ;; Note that messages get dropped when they have been delivered to a delay,
    ;; but the delay is removed in a structure change.
    (is (mail-log=
         [[(h/*R 1)  {:gen-out ["msg-1"]}]
          [(h/*R 2)  {:gen-out ["msg-2"] :del-1-out ["msg-1"]}]
          [(h/*R 3)  {:gen-out ["msg-3"] :del-1-out ["msg-2"]}]
          [(h/*R 4)  {:gen-out ["msg-4"] :del-1-out ["msg-3"]}]
          [(h/*R 5)  {:gen-out ["msg-5"] :del-1-out ["msg-4"]}]
          [(h/*R 6)  {:gen-out ["msg-6"]}]
          [(h/*R 7)  {:gen-out ["msg-7"]}]
          [(h/*R 8)  {:gen-out ["msg-8"] :del-2-out ["msg-6"]}]
          [(h/*R 9)  {:gen-out ["msg-9"]}]
          [(h/*R 10) {:gen-out ["msg-10"]}]
          [(h/*R 11) {:gen-out ["msg-11"] :del-1-out ["msg-10"]}]
          [(h/*R 12) {:gen-out ["msg-12"] :del-1-out ["msg-11"]}]
          [(h/*R 13) {:gen-out ["msg-13"] :del-1-out ["msg-12"]}]
          [(h/*R 14) {:gen-out ["msg-14"] :del-1-out ["msg-13"]}]
          [(h/*R 15) {:gen-out ["msg-15"]}]
          [(h/*R 16) {:gen-out ["msg-16"]}]
          [(h/*R 17) {:gen-out ["msg-17"] :del-2-out ["msg-15"]}]
          [(h/*R 18) {:gen-out ["msg-18"]}]
          [(h/*R 19) {:gen-out ["msg-19"] :del-2-out ["msg-17"]}]
          [(h/*R 20) {:gen-out ["msg-20"]}]]
         (let [gen    (ex/generator (for [i (range)] [(h/*R 1) [(str "msg-" (inc i))]]))
               del-1  (ex/buffer (h/*R 1))
               del-2  (ex/buffer (h/*R 2))
               sc-gen (ex/generator
                       (cycle [[(h/*R 5 -1) [[:disconnect [:gen :out :del-1 :in]]
                                             [:disconnect [:del-1 :out :network :del-1-out]]
                                             [:rem-model :del-1]
                                             [:add-model :del-2 [del-2 h/zero]]
                                             [:connect [:gen :out :del-2 :in]]
                                             [:connect [:del-2 :out :network :del-2-out]]]]
                               [(h/*R 5 -1) [[:disconnect [:gen :out :del-2 :in]]
                                             [:disconnect [:del-2 :out :network :del-2-out]]
                                             [:rem-model :del-2]
                                             [:add-model :del-1 [del-1 h/zero]]
                                             [:connect [:gen :out :del-1 :in]]
                                             [:connect [:del-1 :out :network :del-1-out]]]]]))
               net    (devs/simple-network-model
                       :exec
                       {:gen    [gen h/zero]
                        :del-1  [del-1 h/zero]
                        :sc-gen [sc-gen h/zero]}
                       [[:sc-gen :out :exec :in]
                        [:gen :out :del-1 :in]
                        [:gen :out :network :gen-out]
                        [:del-1 :out :network :del-1-out]])]
           (-> (devs/network-simulator net)
               (devs/afap-root-coordinator :start (h/*R 0) :end (h/*R 20))))))))

(deftest invalid-network-models

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Executive must be an executive model."
                        (devs/network-model :exec
                                            [(ex/buffer (h/*R 5)) h/zero])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"All models in routes must appear in models \(except for :network\)."
                        (devs/network-model :exec
                                            [(devs/simple-executive) h/zero]
                                            {:x (ex/buffer (h/*R 5))}
                                            [[:network :in :y :in]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"A model cannot use the same port for both input and output."
                        (devs/network-model :exec
                                            [(devs/simple-executive) h/zero]
                                            {:x (ex/buffer (h/*R 5))}
                                            [[:network :in :x :in]
                                             [:x :in :network :out]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"A network input port cannot connect directly to a network output port"
                        (devs/network-model :exec
                                            [(devs/simple-executive) h/zero]
                                            {:x (ex/buffer (h/*R 5))}
                                            [[:network :in :network :out]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Executive must be an executive model."
                        (devs/network-model :exec
                                            [(ex/buffer (h/*R 5)) h/zero]
                                            {:x (ex/buffer (h/*R 5))}
                                            [[:network :in :network :out]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Invalid elapsed time for network executive."
                        (devs/network-model :exec
                                            [(devs/simple-executive) 0]
                                            {:x (ex/buffer (h/*R 5))}
                                            [[:network :in :network :out]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Invalid elapsed time for network executive."
                        (devs/network-model :exec
                                            [(devs/simple-executive) (h/*R -1)]
                                            {:x (ex/buffer (h/*R 5))}
                                            [[:network :in :network :out]])))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Invalid elapsed time for network executive."
                        (devs/network-model :exec
                                            [(devs/simple-executive) h/infinity]
                                            {:x (ex/buffer (h/*R 5))}
                                            [[:network :in :network :out]]))))

(deftest rt-test

  #?(:clj
     (let [out (atom [])
           pkg (-> (devs/static-network-model
                    {:buf [(ex/buffer (h/*R 100))
                           h/zero]}
                    [[:network :in :buf :in]
                     [:buf :out :network :out]])
                   devs/network-simulator
                   (devs/rt-root-coordinator
                    :output-fn (partial swap! out into)))]
       (Thread/sleep 100)
       (devs/send-mail! pkg {:in [:one]})
       (Thread/sleep 200)
       (is (= 1 (count @out)))
       (let [[t mail] (first @out)]
         (is (h/<= (h/*R 200) t (h/*R 300)))
         (is (= mail {:out [:one]}))))
     :cljs
     (let [out (atom [])
           pkg (-> (devs/static-network-model
                    {:buf [(ex/buffer (h/*R 100))
                           h/zero]}
                    [[:network :in :buf :in]
                     [:buf :out :network :out]])
                   devs/network-simulator
                   (devs/rt-root-coordinator
                    :output-fn (partial swap! out into)))]
       (async done
              (go
                (async/<! (async/timeout 100))
                (devs/send-mail! pkg {:in [:one]})
                (async/<! (async/timeout 200))
                (is (= 1 (count @out)))
                (let [[t mail] (first @out)]
                  (is (h/<= (h/*R 200) t (h/*R 300)))
                  (is (= mail {:out [:one]})))
                (done))))))

(deftest rt-race-condition-test

  ;; To create an artificial delay, we put a buffer in a network and
  ;; include a route transducer that sleeps. As a result, the
  ;; root-coordinator will still be processing the first send-mail!
  ;; call when the second one is made. We validate that the first one
  ;; completed successfully before the 2nd was processed, because the
  ;; output was what we would expect without all the threads.

  #?(:clj
     (let [out (atom [])
           pkg (-> (devs/static-network-model
                    {:buf [(ex/buffer (h/*R 300))
                           h/zero]}
                    [[:network :in :buf :in (map (fn [msg] (Thread/sleep 100) msg))]
                     [:buf :out :network :out]])
                   devs/network-simulator
                   (devs/rt-root-coordinator
                    :output-fn (partial swap! out into)
                    :paused?   true))]
       (devs/unpause! pkg)
       (future
         (devs/send-mail! pkg {:in [:one]}))
       (future
         (Thread/sleep 10)
         (devs/send-mail! pkg {:in [:two]}))
       (future
         (Thread/sleep 500)
         (devs/pause! pkg))
       (Thread/sleep 600)
       (is (= 1 (count @out)))
       (let [[t mail] (first @out)]
         (is (h/<= (h/*R 300) t (h/*R 400)))
         (is (= mail {:out [:one]}))))))

(deftest no-receiver

  (testing "Simulators shouldn't do anything if there is no receiver for a message."
    (is (mail-log=
         []
         ;; Atomic Simulators don't handle this properly, currently,
         ;; because there is no way to determine if a message is
         ;; applicable (i.e., if a model has a corresponding input
         ;; port).
         #_
         (-> (ex/buffer (h/*R 5))
             devs/atomic-simulator
             (devs/afap-root-coordinator
              :input-log [[(h/*R 10) {:bad ["x"]}]]))
         (-> (devs/static-network-model
              {:buf [(ex/buffer (h/*R 5)) h/zero]}
              [[:network :in :buf :in]
               [:buf :out :network :out]])
             devs/network-simulator
             (devs/afap-root-coordinator
              :input-log [[(h/*R 10) {:bad ["x"]}]]))))))

(deftest step-tests

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Cannot step sim, because time-of-next-event is infinite."
                        (-> (ex/buffer (h/*R 5))
                            devs/atomic-simulator
                            (devs/initialize h/zero)
                            devs/step)))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Synchronization error."
                        (-> (ex/generator (repeat [(h/*R 2 0) ["x"]]))
                            devs/atomic-simulator
                            (devs/initialize h/zero)
                            (devs/step (h/*R -1) {}))))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Synchronization error."
                        (-> (ex/generator (repeat [(h/*R 2 0) ["x"]]))
                            devs/atomic-simulator
                            (devs/initialize h/zero)
                            (devs/step (h/*R 3) {}))))

  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo
                           :cljs ExceptionInfo)
                        #"Sim must be imminent or receiving mail."
                        (-> (ex/generator (repeat [(h/*R 2 0) ["x"]]))
                            devs/atomic-simulator
                            (devs/initialize h/zero)
                            (devs/step (h/*R 1) {})))))
