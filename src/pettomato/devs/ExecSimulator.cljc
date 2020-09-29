(ns pettomato.devs.ExecSimulator)

(defprotocol ExecSimulator
  (get-network-structure [sim] "Returns a network structure object."))
