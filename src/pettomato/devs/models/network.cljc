(ns pettomato.devs.models.network
  (:require
   [clojure.set :refer [subset?]]))

(defn network-model
  "A network model, loosely based on DSDE.

  Barros. Abstract Simulators for the DSDE Formalism. 1998.
  https://repository.lib.ncsu.edu/bitstream/handle/1840.4/6989/1998_0056.pdf"
  [executive-name executive-model]
  {:executive-name  executive-name
   :executive-model executive-model})

(defn network-model? [model]
  (and (map? model)
       (subset? #{:executive-name
                  :executive-model}
                (set (keys model)))))
