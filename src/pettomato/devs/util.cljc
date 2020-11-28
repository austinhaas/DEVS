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

(defn disj-in
  "Like clojure.core/disj for a set in a nested associative structure.
  If the keyword argument :prune? is true, then any empty levels will be
  removed."
  [m ks v & {:keys [prune?]
             :or   {prune? false}}]
  (if (seq ks)
    (let [[k & ks] ks
          v        (disj-in (get m k) ks v :prune? prune?)]
      (if (and prune? (empty? v))
        (dissoc m k)
        (assoc m k v)))
    (disj m v)))

(def ^:dynamic *trace* false)

(defn trace [& args]
  (when *trace*
    (apply log/infof args)))
