(ns pettomato.devs.models.executive
  (:require
   [pettomato.devs.models.atomic :refer [atomic-model]]))

(defn executive-model
  "An executive model, based on DSDE."
  [init int ext con out ta]
  (assert (contains? init :models))
  (assert (contains? init :routes))
  (-> (atomic-model init int ext con out ta)
      (assoc :type ::executive)))

(defn executive-model? [model] (= ::executive (:type model)))
