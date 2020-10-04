(ns pettomato.devs.util
  (:require
   [pettomato.lib.log :as log]))

(def infinity #?(:clj  Double/POSITIVE_INFINITY
                 :cljs (.-POSITIVE_INFINITY js/Number)))

;; https://github.com/weavejester/medley/blob/0.8.1/src/medley/core.cljc#L11
(defn dissoc-in
  "Dissociate a value in a nested assocative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  [m ks]
  (if-let [[k & ks] (seq ks)]
    (if (seq ks)
      (let [v (dissoc-in (get m k) ks)]
        (if (empty? v)
          (dissoc m k)
          (assoc m k v)))
      (dissoc m k))
    m))

(def ^:dynamic *trace* false)

(defn trace [& args]
  (when *trace*
    (apply log/infof args)))
