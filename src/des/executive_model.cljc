(ns des.executive-model
  (:require [des.atomic-model :refer [atomic-model]]))

(defn executive-model [initial-state
                       int-update-fn ext-update-fn con-update-fn
                       output-fn time-advance-fn]
  (-> (atomic-model initial-state int-update-fn ext-update-fn con-update-fn output-fn time-advance-fn)
      (assoc :type ::executive)))

(defn executive? [model] (= ::executive (:type model)))
