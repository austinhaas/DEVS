(ns demo.collision-network
  (:require
   [pt-lib.number :refer [infinity]]
   [devs.executive-model :refer [executive-model]]
   [devs.executive-network-model :refer [executive-network-model]]
   [devs.atomic-model :refer [atomic-model]]
   [demo.script :refer [script]]
   [demo.dispatch :refer [dispatch]]
   [demo.integrator :refer [mult-integrator]]
   [demo.collision-detector :refer [collision-detector]]
   [demo.collision-responder :refer [collision-responder2]]))

(defn collision-network []
  (executive-network-model
   :collision-network
   (executive-model
    {:components  {:int   (mult-integrator 1)
                   :c-det (collision-detector 1)
                   :c-res (collision-responder2)}
     :connections {:N     {[:add :a]   {:int   [:add :a]
                                        :c-det [:add :a]
                                        :c-res [:add :a]}
                           [:add :b]   {:int   [:add :b]
                                        :c-det [:add :b]
                                        :c-res [:add :b]}
                           [:vel :a]   {:int   [:vel :a]
                                        :c-det [:vel :a]
                                        :c-res [:vel :a]}
                           [:vel :b]   {:int   [:vel :b]
                                        :c-det [:vel :b]
                                        :c-res [:vel :b]}}
                   :int   {[:pos :a]   {:N     [:pos :a]}
                           [:pos :b]   {:N     [:pos :b]}}
                   :c-det {:coll-start {:c-res :coll-start
                                        :N     :coll-start}
                           :coll-end   {:N     :coll-end}}
                   :c-res {[:vel :a]   {:int   [:vel :a]
                                        :c-det [:vel :a]}
                           [:vel :b]   {:int   [:vel :b]
                                        :c-det [:vel :b]}}}}
    nil nil nil nil
    (constantly infinity))))
