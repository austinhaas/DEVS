(ns pettomato.devs.models.network-structure
  (:require
   [pettomato.devs.util :refer [dissoc-in]]))

(def parent-name
  "Identifies the parent model in a network."
  :parent)

(def network-name
  "Identifies the network itself in a network. Used to create external
  interfaces."
  :network)

(def empty-network
  "Network structure."
  {:models {}
   :routes {}})

(defn add-model [network model-name model]
  (update network :models assoc model-name model))

(defn rem-model [network model-name]
  (update network :models dissoc model-name))

(defn add-route
  "out-model - A model name.
   out-port  - A model port name.
   in-model  - A model name.
   in-port   - A model port name.
   input-fn  - A function to apply to each message transmitted across this route.

  Note that routes are unique by [out-model out-port in-model in-port]. If
  another route is added with the same values, it will overwrite any previous
  version. You cannot have two routes that differ only in input-fn."
  [network [out-model out-port in-model in-port input-fn]]
  (assoc-in network [:routes out-model out-port in-model in-port] input-fn))

(defn rem-route [network [out-model out-port in-model in-port input-fn]]
  (dissoc-in network [out-model out-port in-model in-port]))

(def get-models :models)

(defn factor-routes
  "Convert the flat, human-readable expression of routes to a nested map, which
  indexes the components for fast access.

  [[k1 p1 k2 p2 f]
   [k1 p3 k3 p4 f]]  -> {k1 {p1 {k2 {p2 f}}}
                             p3 {k3 {p4 f}}}"
  [routes]
  (reduce (fn [m [k1 p1 k2 p2 f]]
            (assert (not= k1 k2) "Direct feedback loops are not allowed.") ;; TMS2000 p. 86.
            (assoc-in m [k1 p1 k2 p2] (or f identity)))
          {}
          routes))

(defn route-messages
  "Returns receiver->port->vs."
  [routes sender->port->vs]
  (let [flattened (for [[sender port->vs]     sender->port->vs
                        [out-port vs]         port->vs
                        [receiver in-port->f] (get-in routes [sender out-port])
                        [in-port f]           in-port->f]
                    [sender out-port receiver in-port f vs])]
    ;; TODO: If several receivers apply the same transducer to the same output
    ;; port, then it might be more efficient to group by [out-port f] and
    ;; ensure that we only apply the function once.
    (reduce (fn [m [sender out-port receiver in-port f vs]]
              (update-in m [receiver in-port] into (map f) vs))
            {}
            flattened)))

(defn flatten-mail
  "k->port->vs -> [[k port v] ...]"
  [m]
  (for [[k port->vs] m
        [port vs]    port->vs
        v            vs]
    [k port v]))

(defn mail-equal? [m1 m2]
  (= (frequencies (flatten-mail m1))
     (frequencies (flatten-mail m2))))

(comment

  (-> empty-network
      (add-model :a nil)
      (add-model :b nil)
      (add-route [:a :out :b :in identity]))

  )
