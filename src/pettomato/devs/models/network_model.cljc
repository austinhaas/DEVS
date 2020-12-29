(ns pettomato.devs.models.network-model
  (:require
   [clojure.set :refer [difference subset?]]))

(defn network-model [models routes]
  (let [keys-in-routes (disj (set (mapcat (fn [[k1 p1 k2 p2 f]]
                                            [k1 k2])
                                          routes))
                             :network)
        keys-in-models (set (keys models))]
    (assert (subset? keys-in-routes keys-in-models)
            (str "These keys in routes were not found in models: "
                 (difference keys-in-routes keys-in-models))))
  {:models models
   :routes routes})

(defn network-model? [model]
  (and (map? model)
       (subset? #{:models :routes}
                (set (keys model)))))
