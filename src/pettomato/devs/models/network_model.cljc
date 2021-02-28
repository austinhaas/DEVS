(ns pettomato.devs.models.network-model
  (:require
   [clojure.set :refer [difference subset?]]))

(defn network-model
  "Creates a network model.

  Args:

    models - A map from names to models.

    routes - A seq of [sk sp rk rp f], where:
      sk - The sender's name.
      sp - The sender's port.
      rk - The receiver's name.
      rp - The receiver's port.
      f  - A function to be applied to every message that traverses this route
           before it is delivered to the receiver.

  Returns:

    A network model."
  [models routes]
  (let [keys-in-routes (disj (set (mapcat (fn [[sk sp rk rp f]]
                                            [sk rk])
                                          routes))
                             :network)
        keys-in-models (set (keys models))]
    (assert (subset? keys-in-routes keys-in-models)
            (str "These keys in routes were not found in models: "
                 (difference keys-in-routes keys-in-models))))
  {:models models
   :routes routes})

(defn network-model?
  "Returns true if model is a network model, otherwise false."
  [model]
  (and (map? model)
       (subset? #{:models :routes}
                (set (keys model)))))
