(ns pettomato.devs.simulators.rt-network-simulator
  "This network simulator is intended for RT use cases. The only difference is
  that it will invoke the transition function for each component simulator each
  time it's own transition function is invoked, even if the component simulators
  are not imminent or receiving mail. This is a form of polling."
  (:require
   [pettomato.devs.models.atomic-model :refer [atomic-model?]]
   [pettomato.devs.models.network-model :refer [network-model?]]
   [pettomato.devs.simulators.rt-atomic-simulator :refer [rt-atomic-simulator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

(declare rt-network-simulator)

(defn default-model->sim
  [model]
  (cond
    (atomic-model?  model) (rt-atomic-simulator  model)
    (network-model? model) (rt-network-simulator model)
    :else                  (throw (ex-info "Unknown model type." {}))))

(defn rt-network-simulator [model]
  (network-simulator model
                     :transition-all? true
                     :model->sim      default-model->sim))
