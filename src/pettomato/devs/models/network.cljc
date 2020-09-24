(ns pettomato.devs.models.network)

(defn network-model
  "A network model based on DSDE.

  Barros. Abstract Simulators for the DSDE Formalism. 1998.
  https://repository.lib.ncsu.edu/bitstream/handle/1840.4/6989/1998_0056.pdf"
  [executive-name executive-model]
  {:executive-name  executive-name
   :executive-model executive-model})

(defn network-model? [model]
  (= ::network (:type model)))
