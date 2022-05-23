(ns pettomato.devs-test
  (:require
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing]])
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.event-log :refer [event-log=]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.models.atomic-model :refer [def-atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(deftest basic-tests

  (testing "Running a very simple atomic simulation."
    (is (event-log= [[(h/*R 2 0)  {:out ["x"]}]
                     [(h/*R 4 0)  {:out ["x"]}]
                     [(h/*R 6 0)  {:out ["x"]}]
                     [(h/*R 8 0)  {:out ["x"]}]
                     [(h/*R 10 0) {:out ["x"]}]]
                    (-> (m/generator (repeat [(h/*R 2 0) ["x"]]))
                        atomic-simulator
                        (afap-root-coordinator :end (h/*R 10 0))))))

  (testing "Specifying a non-zero start time."
    (is (event-log= [[(h/*R 7 0) {:out ["x"]}]
                     [(h/*R 9 0) {:out ["x"]}]]
                    (-> (m/generator (repeat [(h/*R 2 0) ["x"]]))
                        atomic-simulator
                        (afap-root-coordinator :start (h/*R 5 0) :end (h/*R 10 0))))))

  (testing "A simple network."
    (is (event-log= [[(h/*R 15 0) {:out ["x"]}]
                     [(h/*R 25 0) {:out ["y"]}]]
                    (let [gen (m/generator [[(h/*R 10) ["x"]]
                                            [(h/*R 10) ["y"]]])
                          buf (m/buffer (h/*R 5))
                          net (m/static-network-model
                               {:gen [gen h/zero]
                                :buf [buf h/zero]}
                               [[:gen :out :buf :in]
                                [:buf :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest initial-elapsed-tests

  (testing "An atomic simulation."
    (is (event-log= [[(h/*R 5)  {:out ["x"]}]
                     [(h/*R 15) {:out ["y"]}]]
                    (let [gen (m/generator [[(h/*R 10) ["x"]]
                                            [(h/*R 10) ["y"]]])]
                      (-> (atomic-simulator gen :elapsed (h/*R 5))
                          afap-root-coordinator)))))

  (testing "Atomic: tn can't be before start time. "
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (-> (m/generator (repeat [(h/*R 2) ["x"]]))
                     (atomic-simulator :elapsed (h/*R 3))
                     (afap-root-coordinator :end (h/*R 10))))))

  (testing "Atomic: tn can't be equal to start time. "
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (-> (m/generator (repeat [(h/*R 2) ["x"]]))
                     (atomic-simulator :elapsed (h/*R 2))
                     (afap-root-coordinator :end (h/*R 10))))))

  (testing "A network simulation."
    (is (event-log= [[(h/*R 13 0) {:out ["x"]}]
                     [(h/*R 23 0) {:out ["y"]}]]
                    (let [gen (m/generator [[(h/*R 10) ["x"]]
                                            [(h/*R 10) ["y"]]])
                          buf (m/buffer (h/*R 5))
                          net (m/static-network-model
                               {:gen [gen h/zero]
                                :buf [buf h/zero]}
                               [[:gen :out :buf :in]
                                [:buf :out :network :out]])]
                      (-> (network-simulator net :elapsed (h/*R 2))
                          afap-root-coordinator)))))

  (testing "Network: tn can't be before start time."
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (let [gen (m/generator [[(h/*R 10) ["x"]]])
                       net (m/static-network-model
                            {:gen [gen h/zero]}
                            [[:gen :out :network :out]])]
                   (-> (network-simulator net :elapsed (h/*R 20))
                       afap-root-coordinator)))))

  (testing "Network: tn can't be equal start time."
    (is (thrown? #?(:clj  clojure.lang.ExceptionInfo
                    :cljs ExceptionInfo)
                 (let [gen (m/generator [[(h/*R 10) ["x"]]])
                       net (m/static-network-model
                            {:gen [gen h/zero]}
                            [[:gen :out :network :out]])]
                   (-> (network-simulator net :elapsed (h/*R 10))
                       afap-root-coordinator)))))

  (testing "An atomic model within a network simulation."
    (is (event-log= [[(h/*R 13 0) {:out ["x"]}]
                     [(h/*R 23 0) {:out ["y"]}]]
                    (let [gen (m/generator [[(h/*R 10) ["x"]]
                                            [(h/*R 10) ["y"]]])
                          buf (m/buffer (h/*R 5))
                          net (m/static-network-model
                               {:gen [gen (h/*R 2)]
                                :buf [buf h/zero]}
                               [[:gen :out :buf :in]
                                [:buf :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest elapsed-time-tests

  (testing "The model can keep track of total elapsed time."
    (is (event-log= [[(h/*R 15) {:out [1 2 3]}]]
                    (let [gen (m/generator [[(h/*R 5) [[(h/*R 10) 1]]]
                                            [(h/*R 1) [[(h/*R 9)  2]]]
                                            [(h/*R 3) [[(h/*R 6)  3]]]])
                          buf (m/buffer+)
                          net (m/static-network-model
                               {:gen [gen h/zero]
                                :buf [buf h/zero]}
                               [[:gen :out :buf :in]
                                [:buf :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest route-function-tests

  (testing "Using route functions"
    (is (event-log= [[(h/*R 15 0) {:out [["x"]]}]
                     [(h/*R 25 0) {:out [["y"]]}]]
                    (let [gen (m/generator [[(h/*R 10) ["x"]]
                                            [(h/*R 10) ["y"]]])
                          buf (m/buffer+)
                          net (m/static-network-model
                               {:gen [gen h/zero]
                                :buf [buf h/zero]}
                               [[:gen :out :buf :in (map (fn [x] [(h/*R 5) x]))]
                                [:buf :out :network :out (map vector)]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest confluence-tests

  (testing "Internal before external."
    (is (event-log= [[(h/*R 20 0) {:out ["x"]}]
                     [(h/*R 30 0) {:out ["y"]}]]
                    (let [gen (m/generator [[(h/*R 10) ["x"]]
                                            [(h/*R 10) ["y"]]])
                          buf (m/buffer (h/*R 10))
                          net (m/static-network-model
                               {:gen [gen h/zero]
                                :buf [buf h/zero]}
                               [[:gen :out :buf :in]
                                [:buf :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "External before internal."
    (is (event-log= [[(h/*R 20 0) {:out ["x"]}]]
                    (let [gen (m/generator [[(h/*R 10) ["x"]]
                                            [(h/*R 10) ["y"]]])
                          buf (m/buffer2 (h/*R 10))
                          net (m/static-network-model
                               {:gen [gen h/zero]
                                :buf [buf h/zero]}
                               [[:gen :out :buf :in]
                                [:buf :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator))))))

(deftest deeply-nested-structure

  (testing "A trivial deeply nested structure."
    (is (event-log=
         [[(h/*R 5) {:out [[:buf-1 [:buf-2 [:buf-3 0]]]]}]
          [(h/*R 9) {:out [[:buf-1 [:buf-2 [:buf-3 2]]]]}]]
         (let [f   (fn [buf i]
                     (let [id (keyword (str "buf-" i))]
                       (m/static-network-model
                        {id [buf h/zero]}
                        [[:network :in id :in (map (fn [x] [id x]))]
                         [id :out :network :out]])))
               gen (m/generator (for [i (range)] [(h/*R 2) [i]]))
               buf (-> (m/buffer (h/*R 3)) (f 1) (f 2) (f 3))
               net (m/static-network-model
                    {:gen   [gen h/zero]
                     :buf-0 [buf h/zero]}
                    [[:gen :out :buf-0 :in]
                     [:buf-0 :out :network :out]])]
           (-> (network-simulator net)
               (afap-root-coordinator :end (h/*R 10))))))))

(deftest dynamic-network-tests

  (testing "Dynamic behavior."
    (is (event-log=
         [[(h/*R 7 1) {:out [0]}]
          [(h/*R 9 1) {:out [1]}]]
         (let [gen (m/generator
                    [[(h/*R 5) [[:add-model :gen-1 [(m/generator
                                                     (for [i (range)] [(h/*R 2) [i]]))
                                                    h/zero]]
                                [:connect [:gen-1 :out :network :out]]]]
                     [(h/*R 5) [[:disconnect [:gen-1 :out :network :out]]
                                [:rem-model :gen-1]]]])
               net (m/simple-network-model
                    :exec
                    {:gen [gen h/zero]}
                    [[:gen :out :exec :in]])]
           (-> (network-simulator net)
               afap-root-coordinator)))))

  (testing "Dynamic behavior; order doesn't matter."
    (is (event-log=
         [[(h/*R 7 1) {:out [0]}]
          [(h/*R 9 1) {:out [1]}]]
         (let [gen (m/generator [[(h/*R 5) [[:connect [:gen-1 :out :network :out]]
                                            [:add-model :gen-1 [(m/generator
                                                                 (for [i (range)] [(h/*R 2) [i]]))
                                                                h/zero]]]]
                                 [(h/*R 5) [[:rem-model :gen-1]
                                            [:disconnect [:gen-1 :out :network :out]]]]])
               net (m/simple-network-model
                    :exec
                    {:gen [gen h/zero]}
                    [[:gen :out :exec :in]])]
           (-> (network-simulator net)
               afap-root-coordinator)))))

  (testing "Remove an atomic model just before it can emit a message."
    ;; In order to remove :gen before it can send its next message,
    ;; :sc-gen must emit its disconnect message two epsilon
    ;; prior. This is because :exec will take one epsilon to change
    ;; its state, and :gen must be removed at least one epsilon before
    ;; it is scheduled to emit its message.
    (is (event-log= []
                    (let [sc-gen (m/generator [[(h/*R 10 -2) [[:disconnect [:gen :out :network :out]]
                                                              [:rem-model :gen]]]])
                          net    (m/simple-network-model
                                  :exec
                                  {:sc-gen [sc-gen h/zero]
                                   :gen    [(m/generator [[(h/*R 10) ["x"]]])
                                            h/zero]}
                                  [[:sc-gen :out :exec :in]
                                   [:gen :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "Remove an atomic model when it is imminent."
    ;; The message is still sent, because outgoing mail is collected
    ;; and delivered before structure changes are applied.
    (is (event-log= [[(h/*R 10) {:out ["x"]}]]
                    (let [sc-gen (m/generator [[(h/*R 10 -1) [[:disconnect [:gen :out :network :out]]
                                                              [:rem-model :gen]]]])
                          net    (m/simple-network-model
                                  :exec
                                  {:sc-gen [sc-gen h/zero]
                                   :gen    [(m/generator [[(h/*R 10) ["x"]]])
                                            h/zero]}
                                  [[:sc-gen :out :exec :in]
                                   [:gen :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "Remove an atomic model after it is imminent."
    (is (event-log= [[(h/*R 10) {:out ["x"]}]]
                    (let [sc-gen (m/generator [[(h/*R 10 -1) [[:disconnect [:gen :out :network :out]]
                                                              [:rem-model :gen]]]])
                          net    (m/simple-network-model
                                  :exec
                                  {:sc-gen [sc-gen h/zero]
                                   :gen    [(m/generator [[(h/*R 10) ["x"]]])
                                            h/zero]}
                                  [[:sc-gen :out :exec :in]
                                   [:gen :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "Remove a message recipient before it is imminent."
    ;; In this case, :buf will receive the message and process it, but
    ;; it will be removed immediately after, when the structure
    ;; changes are processed.
    (is (event-log= []
                    (let [sc-gen (m/generator [[(h/*R 10 -1) [[:disconnect [:buf :out :network :out]]
                                                              [:disconnect [:gen :out :buf :in]]
                                                              [:rem-model :buf]]]])
                          net    (m/simple-network-model
                                  :exec
                                  {:sc-gen [sc-gen h/zero]
                                   :gen    [(m/generator [[(h/*R 10) ["x"]]])
                                            h/zero]
                                   :buf    [(m/buffer (h/*R 5))
                                            h/zero]}
                                  [[:sc-gen :out :exec :in]
                                   [:gen :out :buf :in]
                                   [:buf :out :network :out]])]
                      (-> (network-simulator net)
                          afap-root-coordinator)))))

  (testing "Moving a sender from one network to another. #1"
    (is (event-log= [[(h/*R 3 1)  {:out-1 ["x"]}]
                     [(h/*R 4 1)  {:out-1 ["x"]}]
                     [(h/*R 5 1)  {:out-1 ["x"]}]
                     [(h/*R 6 1)  {:out-1 ["x"]}]
                     [(h/*R 7 1)  {:out-1 ["x"]}]
                     [(h/*R 8 1)  {:out-2 ["x"]}]
                     [(h/*R 9 1)  {:out-2 ["x"]}]]
                    (let [net-1 (m/simple-network-model
                                 :exec
                                 {:sc-gen [(m/generator [[(h/*R 2) [[:add-model :gen [(m/generator (repeat 10 [(h/*R 1) ["x"]]))
                                                                                      h/zero]]
                                                                    [:connect [:gen :out :network :out]]]]
                                                         [(h/*R 5) [[:rem-model :gen]
                                                                    [:disconnect [:gen :out :network :out]]]]])
                                           h/zero]}
                                 [[:sc-gen :out :exec :in]])
                          net-2 (m/simple-network-model
                                 :exec
                                 {:sc-gen [(m/generator [[(h/*R 7) [[:add-model :gen [(m/generator (repeat 10 [(h/*R 1) ["x"]]))
                                                                                      h/zero]]
                                                                    [:connect [:gen :out :network :out]]]]])
                                           h/zero]}
                                 [[:sc-gen :out :exec :in]])
                          net   (m/static-network-model
                                 {:net-1 [net-1 h/zero]
                                  :net-2 [net-2 h/zero]}
                                 [[:net-1 :out :network :out-1]
                                  [:net-2 :out :network :out-2]])]
                      (-> (network-simulator net)
                          (afap-root-coordinator :end (h/*R 10)))))))

  (testing "Moving a sender from one network to another. #2"
    ;; This works because the second generator is started with an
    ;; elapsed time that matches the time when it was removed from the
    ;; first network.
    (is (event-log= [[(h/*R 7 1)  {:out-1 ["x"]}]
                     [(h/*R 9 1)  {:out-1 ["x"]}]
                     [(h/*R 11 1) {:out-2 ["x"]}]
                     [(h/*R 13 1) {:out-2 ["x"]}]]
                    (let [net-1 (m/simple-network-model
                                 :exec
                                 {:sc-gen [(m/generator [[(h/*R 5) [[:add-model :gen [(m/generator (repeat 10 [(h/*R 2) ["x"]]))
                                                                                      h/zero]]
                                                                    [:connect [:gen :out :network :out]]]]
                                                         [(h/*R 5) [[:rem-model :gen]
                                                                    [:disconnect [:gen :out :network :out]]]]])
                                           h/zero]}
                                 [[:sc-gen :out :exec :in]])
                          net-2 (m/simple-network-model
                                 :exec
                                 {:sc-gen [(m/generator [[(h/*R 10) [[:add-model :gen [(m/generator (repeat 10 [(h/*R 2) ["x"]]))
                                                                                       (h/*R 1)]] ; important
                                                                     [:connect [:gen :out :network :out]]]]])
                                           h/zero]}
                                 [[:sc-gen :out :exec :in]])
                          net   (m/static-network-model
                                 {:net-1 [net-1 h/zero]
                                  :net-2 [net-2 h/zero]}
                                 [[:net-1 :out :network :out-1]
                                  [:net-2 :out :network :out-2]])]
                      (-> (network-simulator net)
                          (afap-root-coordinator :end (h/*R 15)))))))

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
          net-1 (m/simple-network-model
                 :exec
                 {:sc-gen [(m/generator [[(h/*R 5 -2) [[:rem-model :buf]
                                                       [:disconnect [:network :in :buf :in tx1]]
                                                       [:disconnect [:buf :out :network :out]]]]])
                           h/zero]
                  :buf    [(m/buffer (h/*R 1))
                           h/zero]}
                 [[:sc-gen :out :exec :in]
                  [:network :in :buf :in tx1]
                  [:buf :out :network :out]])
          net-2 (m/simple-network-model
                 :exec
                 {:sc-gen [(m/generator [[(h/*R 5 -2) [[:add-model :buf [(m/buffer (h/*R 1))
                                                                         h/zero]]
                                                       [:connect [:network :in :buf :in tx2]]
                                                       [:connect [:buf :out :network :out]]]]])
                           h/zero]}
                 [[:sc-gen :out :exec :in]])
          net   (m/static-network-model
                 {:gen   [(m/generator (for [i (range 10)] [(h/*R 2) [(str "msg-" i)]]))
                          h/zero]
                  :net-1 [net-1 h/zero]
                  :net-2 [net-2 h/zero]}
                 [[:gen :out :net-1 :in]
                  [:gen :out :net-2 :in]
                  [:net-1 :out :network :out-1]
                  [:net-2 :out :network :out-2]])]
      (is (event-log=
           [[(h/*R 3) {:out-1 ["msg-0"]}]
            ;; msg-1 is lost, because we didn't preserve the buffer's
            ;; state when we moved it.
            [(h/*R 7) {:out-2 ["msg-2"]}]
            [(h/*R 9) {:out-2 ["msg-3"]}]]
           (-> (network-simulator net)
               (afap-root-coordinator :end (h/*R 10))
               doall)))
      (is (= [["msg-0" "msg-1"] ["msg-2" "msg-3" "msg-4"]]
             [@a1 @a2]))))

  (testing "Add and remove a NETWORK model."
    (is (event-log=
         [[(h/*R 13) {:out [5]}]
          [(h/*R 15) {:out [6]}]
          [(h/*R 17) {:out [7]}]
          [(h/*R 19) {:out [8]}]]
         (let [sc-gen (m/generator
                       [[(h/*R 10) [[:add-model :net-1 [(m/simple-network-model
                                                         :exec
                                                         {:buf [(m/buffer (h/*R 1)) h/zero]}
                                                         [[:network :in :buf :in]
                                                          [:buf :out :network :out]])
                                                        h/zero]]
                                    [:connect [:gen :out :net-1 :in]]
                                    [:connect [:net-1 :out :network :out]]]]
                        [(h/*R 10) [[:rem-model :net-1]
                                    [:disconnect [:gen :out :net-1 :in]]
                                    [:disconnect [:net-1 :out :network :out]]]]])
               gen    (m/generator (for [i (range)] [(h/*R 2) [i]]))
               net    (m/simple-network-model
                       :exec
                       {:sc-gen [sc-gen h/zero]
                        :gen    [gen    h/zero]}
                       [[:sc-gen :out :exec :in]])]
           (-> (network-simulator net)
               (afap-root-coordinator :end (h/*R 30)))))))

  (testing "ad-hoc network structure change test"
    ;; Note that messages get dropped when they have been delivered to a delay,
    ;; but the delay is removed in a structure change.
    (is (event-log= [[(h/*R 1)  {:gen-out ["msg-1"]}]
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
                    (let [gen    (m/generator (for [i (range)] [(h/*R 1) [(str "msg-" (inc i))]]))
                          del-1  (m/buffer (h/*R 1))
                          del-2  (m/buffer (h/*R 2))
                          sc-gen (m/generator
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
                          net    (m/simple-network-model
                                  :exec
                                  {:gen    [gen h/zero]
                                   :del-1  [del-1 h/zero]
                                   :sc-gen [sc-gen h/zero]}
                                  [[:sc-gen :out :exec :in]
                                   [:gen :out :del-1 :in]
                                   [:gen :out :network :gen-out]
                                   [:del-1 :out :network :del-1-out]])]
                      (-> (network-simulator net)
                          (afap-root-coordinator :start (h/*R 0) :end (h/*R 20))))))))
