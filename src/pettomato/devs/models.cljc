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

(defn initial-state   [model] (:initial-state   model))
(defn int-update-fn   [model] (:int-update-fn   model))
(defn ext-update-fn   [model] (:ext-update-fn   model))
(defn con-update-fn   [model] (:con-update-fn   model))
(defn output-fn       [model] (:output-fn       model))
(defn time-advance-fn [model] (:time-advance-fn model))

(defn executive-model [init int ext con out ta]
  (-> (atomic-model init int ext con out ta)
      (assoc :type ::executive)))

(defn register
  "Register the model with the key in state."
  [state k model]
  (assoc-in state [:components k] model))

(defn unregister
  "Unregister the model with the key in state."
  [state k]
  (dissoc-in state [:components k]))

(defn get-components
  "Returns a map from keys to models."
  [state]
  (:components state))

(defn connect
  "Add a connection to state s between key k1, port p1 and key k2, port p2.

  An optional transducer can be supplied, which will be applied to each value
  that passes across this connection."
  ([s k1 p1 k2 p2]
   (connect s k1 p1 k2 p2 identity))
  ([s k1 p1 k2 p2 xform]
   (assert (nil? (get-in s [:connections k1 p1 k2 p2])) (str "There is already a connection between these ports: " [k1 p1] " " [k2 p2]))
   (assoc-in s [:connections k1 p1 k2 p2] xform)))

(defn disconnect
  "Remove a connection from state s between key k1, port p1 and key k2, port p2."
  [s k1 p1 k2 p2]
  (dissoc-in s [:connections k1 p1 k2 p2]))

(defn get-connections
  "Returns a seq of [k p xform], where k, p is the key, port of the receiver and
  xform is the transducer to apply to each message."
  [s k1 p1]
  (for [[k m]     (get-in s [:connections k1 p1])
        [p xform] m]
    [k p xform]))

(def network-id
  "Represents the network in connections."
  ::N)

(defn network-model [exec-name exec-model]
  (assert (executive? exec-model))
  {:type            ::network
   :executive-name  exec-name
   :executive-model exec-model})

(defn exec-name  [model] (:executive-name  model))
(defn exec-model [model] (:executive-model model))
