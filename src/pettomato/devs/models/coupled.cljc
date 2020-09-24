(ns pettomato.devs.models.coupled
  (:require
   [clojure.set :refer [subset?]]))

(defn coupled-model
  "A coupled model for Parallel DEVS with Ports.

  \"The DEVS formalism includes the means to build models from components.\"
    - Ziegler, et al. Theory of Modeling and Simulation. 2000. Ch. 4.2.3.

  models - A map from model names to models.

  routes - A seq of [out-model-name out-port in-model-name in-port f], where f
  is an optional function to apply to each value that travels this route."
  [models routes]
  {:models models
   :routes routes})

(defn coupled-model? [model]
  (and (map? model)
       (subset? #{:models
                  :routes}
                (set (keys model)))))
