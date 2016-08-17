(ns demo.db-test
  (:require
   [clojure.test :refer :all]
   [test-util :refer [eq?]]
   [devs.atomic-simulator :refer [atomic-simulator]]
   [devs.fast-as-possible-system :refer [fast-as-possible-system]]
   [demo.db :refer [db]]))

(defn- normalize-results [results]
  (mapv (fn [[t [port [query val]]]]
          [t [port [query (set val)]]])
        results))

(def f #(-> (atomic-simulator (db :id :x))
            (fast-as-possible-system 0 1000 %)
            normalize-results))

(deftest db-basic-tests
  (testing "Primary key query with empty response."
    (is (eq? (f [[10 [[:query :q] {:id :a}]]])
             [[10 [[:query-response :q] [{:id :a} #{}]]]])))

  (testing "Primary key query with single response."
    (is (eq? (f [[0  [:insert {:id :a}]]
                 [10 [[:query :q] {:id :a}]]])
             [[10 [[:query-response :q] [{:id :a} #{{:id :a}}]]]])))

  (testing "Various queries."
    (is (eq? (f [[0  [:insert {:id :a :x 0 :y 1}]]
                 [0  [:insert {:id :b :x 0 :y 1}]]
                 [0  [:insert {:id :c :x 1 :y 2}]]
                 [10 [[:query :q] {:id :a}]]
                 [20 [[:query :r] {:x 0}]]
                 [30 [[:query :s] {:y 2}]]
                 [40 [[:query :t] {:x odd?}]]])
             [[10 [[:query-response :q] [{:id :a} #{{:id :a :x 0 :y 1}}]]]
              [20 [[:query-response :r] [{:x 0} #{{:id :a :x 0 :y 1} {:id :b :x 0 :y 1}}]]]
              [30 [[:query-response :s] [{:y 2} #{{:id :c :x 1 :y 2}}]]]
              [40 [[:query-response :t] [{:x odd?} #{{:id :c :x 1 :y 2}}]]]])))

  (testing "Delete by primary key."
    (is (eq? (f [[0  [:insert {:id :a :x 0 :y 1}]]
                 [10 [:delete {:id :a}]]
                 [20 [[:query :q] {:id :a}]]
                 [30 [[:query :q] {:x :0}]]
                 [40 [[:query :q] {:y :1}]]])
             [[20 [[:query-response :q] [{:id :a} #{}]]]
              [30 [[:query-response :q] [{:x :0} #{}]]]
              [40 [[:query-response :q] [{:y :1} #{}]]]])))

  (testing "Delete by indexed attribute."
    (is (eq? (f [[0  [:insert {:id :a :x 0 :y 1}]]
                 [10 [:delete {:x 0}]]
                 [20 [[:query :q] {:id :a}]]
                 [30 [[:query :q] {:x :0}]]
                 [40 [[:query :q] {:y :1}]]])
             [[20 [[:query-response :q] [{:id :a} #{}]]]
              [30 [[:query-response :q] [{:x :0} #{}]]]
              [40 [[:query-response :q] [{:y :1} #{}]]]])))

  (testing "Delete by non-indexed attribute."
    (is (eq? (f [[0  [:insert {:id :a :x 0 :y 1}]]
                 [10 [:delete {:y 1}]]
                 [20 [[:query :q] {:id :a}]]
                 [30 [[:query :q] {:x :0}]]
                 [40 [[:query :q] {:y :1}]]])
             [[20 [[:query-response :q] [{:id :a} #{}]]]
              [30 [[:query-response :q] [{:x :0} #{}]]]
              [40 [[:query-response :q] [{:y :1} #{}]]]])))

  (testing "Delete using predicate fn."
    (is (eq? (f [[0  [:insert {:id :a :x 1 :y 1}]]
                 [0  [:insert {:id :b :x 2 :y 1}]]
                 [0  [:insert {:id :c :x 3 :y 2}]]
                 [10 [:delete {:x odd?}]]
                 [20 [[:query :q] {}]]])
             [[20 [[:query-response :q] [{} #{{:id :b :x 2 :y 1}}]]]])))

  (testing "Modify by primary key."
    (is (eq? (f [[0  [:insert {:id :a :x 0 :y 1}]]
                 [10 [:modify [{:x 1} {:id :a}]]]
                 [20 [[:query :q] {:id :a}]]])
             [[20 [[:query-response :q] [{:id :a} #{{:id :a :x 1 :y 1}}]]]])))

  (testing "Modify by indexed attribute."
    (is (eq? (f [[0  [:insert {:id :a :x 0 :y 1}]]
                 [0  [:insert {:id :b :x 0 :y 1}]]
                 [0  [:insert {:id :c :x 1 :y 2}]]
                 [10 [:modify [{:x 1} {:x 0}]]]
                 [20 [[:query :q] {:x 1}]]])
             [[20 [[:query-response :q] [{:x 1} #{{:id :a :x 1 :y 1}
                                                  {:id :b :x 1 :y 1}
                                                  {:id :c :x 1 :y 2}}]]]])))

  (testing "Modify by non-indexed attribute."
    (is (eq? (f [[0  [:insert {:id :a :x 0 :y 1}]]
                 [0  [:insert {:id :b :x 0 :y 1}]]
                 [0  [:insert {:id :c :x 1 :y 2}]]
                 [10 [:modify [{:x 1} {:y 1}]]]
                 [20 [[:query :q] {:x 1}]]])
             [[20 [[:query-response :q] [{:x 1} #{{:id :a :x 1 :y 1}
                                                  {:id :b :x 1 :y 1}
                                                  {:id :c :x 1 :y 2}}]]]]))))

(deftest db-subscription-tests
  (is (eq? (f [[0  [[:sub :q] [{:x 0} {:y identity}]]]
               [10 [:insert {:id :a :x 0 :y 1}]]
               [10 [:insert {:id :b :x 0 :y 1}]]
               [10 [:insert {:id :c :x 1 :y 2}]]
               [20 [:modify [{:y 2} {:x 0}]]]
               [30 [[:sub :r] [{} {:y odd?}]]]
               [40 [:modify [{:y 3} {:id :a}]]]
               [50 [:delete {:id :b}]]
               [60 [[:unsub :q] [{:x 0} {:y identity}]]]
               [70 [:modify [{:y 4} {:id :a}]]]])
           [[10 [[:sub-response :q] [[{:x 0} {:y identity}]
                                     #{{:old {}
                                        :new {:id :a, :x 0, :y 1}
                                        :oldf {}
                                        :newf {:y 1}}}]]]
            [10 [[:sub-response :q] [[{:x 0} {:y identity}]
                                     #{{:old {}
                                        :new {:id :b, :x 0, :y 1}
                                        :oldf {}
                                        :newf {:y 1}}}]]]
            [20 [[:sub-response :q] [[{:x 0} {:y identity}]
                                     #{{:old {:id :a, :x 0, :y 1}
                                        :new {:id :a, :x 0, :y 2}
                                        :oldf {:y 1}
                                        :newf {:y 2}}
                                       {:old {:id :b, :x 0, :y 1}
                                        :new {:id :b, :x 0, :y 2}
                                        :oldf {:y 1}
                                        :newf {:y 2}}}]]]
            [30 [[:sub-response :r] [[{} {:y odd?}]
                                     #{{:old {}
                                        :new {:id :a, :x 0, :y 2}
                                        :oldf {}
                                        :newf {:y false}}
                                       {:old {}
                                        :new {:id :b, :x 0, :y 2}
                                        :oldf {}
                                        :newf {:y false}}
                                       {:old {}
                                        :new {:id :c, :x 1, :y 2}
                                        :oldf {}
                                        :newf {:y false}}}]]]
            [40 [[:sub-response :q] [[{:x 0} {:y identity}]
                                     #{{:old {:id :a, :x 0, :y 2}
                                        :new {:id :a, :x 0, :y 3}
                                        :oldf {:y 2}
                                        :newf {:y 3}}}]]]
            [40 [[:sub-response :r] [[{} {:y odd?}]
                                     #{{:old {:id :a, :x 0, :y 2}
                                        :new {:id :a, :x 0, :y 3}
                                        :oldf {:y false}
                                        :newf {:y true}}}]]]
            [50 [[:sub-response :r] [[{} {:y odd?}]
                                     #{{:old {:id :b, :x 0, :y 2}
                                        :new {}
                                        :oldf {:y false}
                                        :newf {}}}]]]
            [50 [[:sub-response :q] [[{:x 0} {:y identity}]
                                     #{{:old {:id :b, :x 0, :y 2}
                                        :new {}
                                        :oldf {:y 2}
                                        :newf {}}}]]]
            [70 [[:sub-response :r] [[{} {:y odd?}]
                                     #{{:old {:id :a, :x 0, :y 3}
                                        :new {:id :a, :x 0, :y 4}
                                        :oldf {:y true}
                                        :newf {:y false}}}]]]])))
