(ns pettomato.devs.models.network-model
  (:require
   [clojure.set :refer [difference intersection subset?]]))

(defn validate-network-model!
  "Throws an exception if model is invalid."
  [model]
  (let [{:keys [models routes]} model]
    ;; A network must contain at least one component model.
    (when (zero? (count models))
      (throw (ex-info "A network model must contain at least one component model."
                      model)))
    ;; All models in routes must appear in models (except for :network).
    (let [keys-in-routes (-> (mapcat (fn [[sk _ rk _ _]] [sk rk])
                                     routes)
                             set
                             (disj :network))
          keys-in-models (-> models
                             keys
                             set)]
     (when-not (subset? keys-in-routes keys-in-models)
       (throw (ex-info (str "All models in routes must appear in models (except for :network). Unknown models in routes: "
                            (difference keys-in-routes keys-in-models))
                       model))))
    ;; A model cannot use the same port for both input and output.
    (let [input-ports  (set (for [[sk sp _ _ _] routes] [sk sp]))
          output-ports (set (for [[_ _ rk rp _] routes] [rk rp]))]
      (when (seq (intersection input-ports output-ports))
        (throw (ex-info "A model cannot use the same port for both input and output."
                        model))))
    ;; A network input port cannot connect directly to a network output port.
    (let [pass-thru-routes (filter (fn [[sk _ rk _ _]] (= sk rk))
                                   routes)]
      (when (seq pass-thru-routes)
        (throw (ex-info (str "A network input port cannot connect directly to a network output port: "
                             pass-thru-routes)
                        model)))))
  nil)

(defn network-model
  "Creates a network model.

  Args:

    models - A map from names to models.

    routes - A seq of [sk sp rk rp f], where:
      sk - The sender's name.
      sp - The sender's port.
      rk - The receiver's name.
      rp - The receiver's port.
      f  - (Optional) A function to apply to every message that traverses
           this route before it is delivered to the receiver.

  Returns:

    A network model."
  [models routes]
  (let [model {:models models
               :routes routes}]
    (validate-network-model! model)
    model))

(defn network-model?
  "Returns true if model is a network model, otherwise false."
  [model]
  (and (map? model)
       (subset? #{:models :routes}
                (set (keys model)))))
