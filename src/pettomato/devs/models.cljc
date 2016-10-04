(ns pettomato.devs.models
  (:require
   [pettomato.devs.util :refer [dissoc-in]]))

(defn atomic?    [model] (or (= ::atomic (:type model))
                                      (= ::executive (:type model))))
(defn executive? [model] (= ::executive (:type model)))
(defn network?   [model] (= ::network (:type model)))

(defn atomic-model [init int ext con out ta]
  (assert (or (nil? int) (ifn? int)))
  (assert (or (nil? ext) (ifn? ext)))
  (assert (or (nil? con) (ifn? con)))
  (assert (or (nil? out) (ifn? out)))
  (assert (or (nil? ta)  (ifn? ta)))
  {:type            ::atomic
   :initial-state   init
   :int-update-fn   int
   :ext-update-fn   ext
   :con-update-fn   (if (nil? con)
                      (fn [s e x] (ext (int s) 0 x))
                      con)
   :output-fn       out
   :time-advance-fn ta})

(defn initial-state   [m] (:initial-state   m))
(defn int-update-fn   [m] (:int-update-fn   m))
(defn ext-update-fn   [m] (:ext-update-fn   m))
(defn con-update-fn   [m] (:con-update-fn   m))
(defn output-fn       [m] (:output-fn       m))
(defn time-advance-fn [m] (:time-advance-fn m))

(defn executive-model [init int ext con out ta]
  (assert (contains? init :components))
  (assert (contains? init :connections))
  (-> (atomic-model init int ext con out ta)
      (assoc :type ::executive)))

(defn register [s k m]
  (assoc-in s [:components k] m))

(defn unregister [s k]
  (dissoc-in s [:components k]))

(defn get-components [s]
  (:components s))

(defn connect
  ([s k1 p1 k2 p2 t]
   (assert (nil? (get-in s [:connections k1 p1 k2 p2])) (str "There is already a connection between these ports: " [k1 p1] " " [k2 p2]))
   (assoc-in s [:connections k1 p1 k2 p2] t))
  ([s k1 p1 k2 p2]   (connect s k1 p1 k2 p2 identity)))

(defn disconnect [s k1 p1 k2 p2]
  (dissoc-in s [:connections k1 p1 k2 p2]))

(defn get-connections [s k1 p1]
  (for [[k m] (get-in s [:connections k1 p1])
        [p t] m]
    [k p t]))

(defn network-model [exec-name exec-model]
  (assert (executive? exec-model))
  {:type            ::network
   :executive-name  exec-name
   :executive-model exec-model})

(defn exec-name  [m] (:executive-name  m))
(defn exec-model [m] (:executive-model m))
