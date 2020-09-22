(ns pettomato.devs.dsdevs.models.executive
  (:require
   [pettomato.devs.parallel.models.atomic :as a]
   [pettomato.devs.parallel.models.coupled :as c]))

;; This is a dupe, and we are going to have problems, so maybe this is something
;; that should be fixed.
(def network-id
  "A symbol that is used to refer to the network in an external coupling."
  ::N)


(defn add-component
  "Add model with key k to the network."
  [network k model]
  (assoc-in network [:components k] model))

(defn remove-component
  "Remove the model with key k from the network."
  [network k]
  (dissoc-in network [:components k]))

(defn get-components
  "Returns a map from keys to models."
  [network]
  (:components network))

(defn connect
  "Add a connection to network from key k1, port p1 to key k2, port p2.

  An optional transducer can be supplied, which will be applied to each value
  that passes across this connection."
  ([network k1 p1 k2 p2]
   (connect network k1 p1 k2 p2 identity))
  ([network k1 p1 k2 p2 xform]
   (assert (nil? (get-in network [:connections k1 p1 k2 p2]))
           (str "There is already a connection between these ports: " [k1 p1] " " [k2 p2]))
   (assoc-in network [:connections k1 p1 k2 p2] xform)))

(defn disconnect
  "Remove the connection from network from key k1, port p1 to key k2, port p2."
  [network k1 p1 k2 p2]
  (dissoc-in network [:connections k1 p1 k2 p2]))

;; I don't think this is used.
(defn get-connections
  "Returns all of the outbound connections for a given key and port in a network.

  Returns a seq of [k p xform], where k, p is the key, port of the receiver and
  xform is a transducer to apply to each message."
  [network k1 p1]
  (for [[k m]     (get-in network [:connections k1 p1])
        [p xform] m]
    [k p xform]))

(defn executive-model [init int ext con out ta]
  (-> (a/atomic-model init int ext con out ta)
      (assoc :type    ::executive
             :network (c/coupled-model {} []))))

(defn executive-model? [model] (= ::executive (:type model)))
