(ns pettomato.devs.models
  (:require
   [clojure.spec.alpha :as s]))

;;; Atomic model
;;; ----------------------------------------------------------------------------

;; An atomic model for a dynamic implementation Parallel DEVS with Ports.

(s/def ::elapsed-time (s/and number? (partial <= 0)))

(s/def ::initial-total-state (s/tuple any? ::elapsed-time))

;; I don't think we can spec these functions, because they have to be broad
;; enough for the entire space of implementations, and that isn't enough
;; information for the spec implementation, which is based on generative
;; testing, to handle.
(s/def ::int-update (s/nilable ifn?))
(s/def ::ext-update (s/nilable ifn?))
(s/def ::con-update (s/nilable ifn?))
(s/def ::output (s/nilable ifn?))
(s/def ::time-advance ifn? #_(s/fspec :args any?
                                      :ret number?))

(s/def ::atomic-model (s/keys :req-un [::initial-total-state
                                       ::time-advance]
                              :opt-un [::int-update
                                       ::ext-update
                                       ::con-update
                                       ::output]))

(defn atomic-model [initial-total-state int-update ext-update con-update output time-advance]
  {:initial-total-state initial-total-state
   :int-update          int-update
   :ext-update          ext-update
   :con-update          (or con-update (fn [s e x] (ext-update (int-update s) 0 x)))
   :output              output
   :time-advance        time-advance})

;;; Network model
;;; ----------------------------------------------------------------------------

(def network-name
  "A name that is used to refer to the network in an external coupling."
  ::N)

(s/def ::model-name any?)

(s/def ::port-name any?)

(s/def ::models (s/map-of ::model-name (s/or :atomic  ::atomic-model
                                             :network ::network-model)))

(s/def ::route (s/cat :in-name  ::model-name
                      :in-port  ::port-name
                      :out-name ::model-name
                      :out-port ::port-name
                      :f        (s/? ifn?)))

(s/def ::routes (s/coll-of ::route))

;;(s/def ::apply-state-changes ifn?)

(s/def ::network-model (s/keys :req-un [::models ::routes;; ::apply-state-changes
                                        ]))

(defn network-model [models routes]
  {:models models
   :routes routes})

;; ----------------------------------------------------------------------------

(comment

  (s/explain ::atomic-model {:initial-total-state [nil 0]
                             :int-update          identity
                             :ext-update          identity
                             :con-update          identity
                             :output              identity
                             :time-advance        :sigma})
  )
