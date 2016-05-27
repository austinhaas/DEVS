(ns des.test
  (:require
   [clojure.test :refer :all]
   [clojure.core.match :refer [match]]
   [clojure.core.async :as async :refer [chan go <! timeout close! >!]]
   [pt-lib.number :refer [infinity]]
   [des.model :refer [model network]]
   [des.network-sim :refer [network-sim]]
   [des.real-time-system :refer [real-time-system]]))

(defn generator [period]
  (model ["active" period]
         (fn [s] (let [[phase sigma] s]
                   ["active" period]))
         nil
         nil
         (fn [s] (let [[phase sigma] s]
                   (case phase
                     "active" [['out 1]])))
         (fn [s] (let [[phase sigma] s]
                   sigma))))

(defn switch [processing-time]
  (model
   ["passive" infinity 'in #{} true]
   (fn [s]
     (let [[phase sigma inport store Sw] s]
       ["passive" infinity inport store Sw]))
   (fn [s e x]
     (let [[phase sigma inport store Sw] s
           [p v]                         x]
       (if (= phase "passive")
         ["busy" processing-time p v (not Sw)]
         [phase (- sigma e) inport store Sw])))
   nil
   (fn [s]
     (let [[phase sigma inport store Sw] s]
       (match [phase Sw inport]
         ["busy" true  'in ] [['out  store]]
         ["busy" true  'in1] [['out1 store]]
         ["busy" false 'in ] [['out1 store]]
         ["busy" false 'in1] [['out  store]])))
   (fn [s]
     (let [[phase sigma inport store Sw] s]
       sigma))))

(defn simple-delay-component [processing-time]
  (model
   ["passive" infinity 1]
   (fn [s]
     (let [[phase sigma store] s]
       ["passive" infinity store]))
   (fn [s e x]
     (assert (= 1 (count x)))
     (let [[phase sigma store] s
           [p v]               (first x)]
       (if (= phase "passive")
         ["busy" processing-time v]
         ["passive" (- sigma e) store])))
   nil
   (fn [s]
     (let [[phase sigma store] s]
       [['out store]]))
   (fn [s]
     (let [[phase sigma store] s]
       sigma))))

(def n (network
        {:delay-1 (simple-delay-component 5000)}
        {:N       {'in  {:delay-1 'in}}
         :delay-1 {'out {:N       'out}}}))

(def n1 (network
         {:delay-1 (simple-delay-component 1000)
          :delay-2 (simple-delay-component 1000)}
         {:N       {'in  {:delay-1 'in}}
          :delay-1 {'out {:delay-2 'in}}
          :delay-2 {'out {:N       'out}}}))

(def n2 (network
         {:gen     (generator 1000)
          :delay-1b (simple-delay-component 1000)
          :delay-2b (simple-delay-component 2000)
          :delay-3 n1
          :switch  (switch 1000)}
         {:N       {'in   {:switch  'in}}
          :delay-1b {'out  {:N       'out1}}
          :delay-3 {'out  {:N       'out2}}
          :gen     {'out  {:switch  'in}}
          :switch  {'out  {:delay-1b 'in}
                    'out1 {:delay-2b 'in}}
          :delay-2b {'out  {:delay-3 'in}}}))

(def sim (network-sim n1))

(def chan-in  (chan 1))
(def chan-out (chan 1))

(real-time-system sim 0 chan-in chan-out)

(go (loop []
      (let [v (<! chan-out)]
        (if v
          (do (println "> " v)
              (recur))
          (println 'done)))))

#_(go (>! chan-in [['in 1]]))

#_(close! chan-in)
