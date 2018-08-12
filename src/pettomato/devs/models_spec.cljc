(ns pettomato.devs.models-spec
  (:require
   [clojure.spec :as s]
   [pettomato.devs.util :refer [infinity]]))

(s/def ::type #{::atomic ::executive ::network})

(s/def ::state ::s/any)

(s/def ::port-id keyword?)

(s/def ::port-label ::s/any)

(s/def ::port (s/or :simple ::port-id :labeled (s/cat :port ::port-id :label ::port-label)))

(s/def ::val ::s/any)

(s/def ::event (s/cat :port ::port :val ::val))

(s/def ::events (s/nilable (s/coll-of ::event)))

(s/def ::elapsed number?)

(s/def ::init ::s/any)

(s/fdef ::int
        :args (s/cat :s ::state)
        :ret  ::s/any)

(s/fdef ::ext
        :args (s/cat :s ::state :e ::elapsed :x ::events)
        :ret  ::state)

(s/fdef ::con
        :args (s/cat :s ::state :e ::elapsed :x ::events)
        :ret  ::state)

(s/fdef ::out
        :args (s/cat :s ::state)
        :ret  ::events)

(s/fdef ::ta
        :args (s/cat :s ::state)
        :ret  (s/and number? #(>= % 0)))

(s/def ::atomic-model (s/keys :req [::type ::ta]
                              :opt [::int ::ext ::con ::out]))

(s/def ::component-id keyword?)

(s/def ::components (s/map-of ::component-id ::model))

(s/def ::connections ::s/any) ;; TODO

(s/def ::executive-model (s/merge ::atomic-model
                                  (s/keys :req [::components ::connections])))

(s/def ::exec-name keyword?)
(s/def ::exec-model ::executive-model)

(s/def ::network-model (s/keys :req [::exec-name ::exec-model]))

(defmulti model-type ::type)

(defmethod model-type ::atomic    [_] ::atomic-model)
(defmethod model-type ::executive [_] ::executive-model)
(defmethod model-type ::network   [_] ::network-model)

(s/def ::model (s/multi-spec model-type ::type))
