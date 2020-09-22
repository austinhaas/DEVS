(ns pettomato.devs.models.coupled
  "A coupled model for Parallel DEVS.

  \"The DEVS formalism includes the means to build models from components.\"
    - Ziegler, et al. Theory of Modeling and Simulation. 2nd Ed. Ch. 4.2.3.")

(def network-id
  "A symbol that is used to refer to the network in an external coupling."
  ::N)

(defn coupled-model
  "A coupled model for Parallel DEVS with Ports.

  models - A map from model names to models.

  routes - A seq of [out-model-name out-port in-model-name in-port xform], where
  xform is optional."
  [models routes]
  {:type   ::coupled
   :models models
   :routes (reduce (fn [m [k1 p1 k2 p2 xform]]
                     (assert (not= k1 k2) "Direct feedback loops are not allowed.") ;; TMS p. 86.
                     (assoc-in m [k1 p1 k2 p2] (or xform identity)))
                   {}
                   routes)})

(defn models [model] (:models model))
(defn routes [model] (:routes model))

(defn coupled-model? [model] (= ::coupled (:type model)))
