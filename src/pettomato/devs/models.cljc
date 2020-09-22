(ns pettomato.devs.models
  "Abstract models for Parallel DEVS / DSDE.")

;;; Atomic model
;;; ----------------------------------------------------------------------------

(defn atomic-model
  "An atomic model for Parallel DEVS with Ports.

  init - A total state: [inital-state elapsed-time]."
  [init int ext con out ta]
  (assert (or (nil? int) (ifn? int)))
  (assert (or (nil? ext) (ifn? ext)))
  (assert (or (nil? con) (ifn? con)))
  (assert (or (nil? out) (ifn? out)))
  (assert (or (nil? ta)  (ifn? ta)))
  {:type            ::atomic
   :initial-state   init
   :int-update-fn   int
   :ext-update-fn   ext
   :con-update-fn   (if (nil? con)
                      (fn [s e x] (ext (int s) 0 x))
                      con)
   :output-fn       out
   :time-advance-fn ta})

(defn initial-state   [model] (:initial-state   model))
(defn int-update-fn   [model] (:int-update-fn   model))
(defn ext-update-fn   [model] (:ext-update-fn   model))
(defn con-update-fn   [model] (:con-update-fn   model))
(defn output-fn       [model] (:output-fn       model))
(defn time-advance-fn [model] (:time-advance-fn model))

(defn atomic-model? [model] (= ::atomic (:type model)))

;;; Coupled model
;;; ----------------------------------------------------------------------------

(def network-name
  "A name that is used to refer to the network in an external coupling."
  ::N)

(defn coupled-model
  "A coupled model for Parallel DEVS with Ports.

  \"The DEVS formalism includes the means to build models from components.\"
    - Ziegler, et al. Theory of Modeling and Simulation. 2000. Ch. 4.2.3.

  models - A map from model names to models.

  routes - A seq of [out-model-name out-port in-model-name in-port f], where f
  is an optional function to apply to each value that travels this route."
  [models routes]
  {:type   ::coupled
   :models models
   :routes (reduce (fn [m [k1 p1 k2 p2 f]]
                     (assert (not= k1 k2) "Direct feedback loops are not allowed.") ;; TMS2000 p. 86.
                     (assoc-in m [k1 p1 k2 p2] (or f identity)))
                   {}
                   routes)})

(defn models [model] (:models model))
(defn routes [model] (:routes model))

(defn coupled-model? [model] (= ::coupled (:type model)))

;;; Executive model (DSDE)
;;; ----------------------------------------------------------------------------

;; How do we make the network part of the state?

;; The executive model cannot be part of the network.

;; The network cannot influence itself. (Can other components?)

;; Barros includes a "structure function": y : Sx -> Σ*, where Σ* is the set of
;; network structures.

(defn executive-model
  "An executive model, based on DSDE."
  [init int ext con out ta]
  (assert (contains? init :models))
  (assert (contains? init :routes))
  (-> (atomic-model init int ext con out ta)
      (assoc :type ::executive)))

(defn executive-model? [model] (= ::executive (:type model)))

;;; Network model (DSDE)
;;; ----------------------------------------------------------------------------

(defn network-model
  "A network model based on DSDE.

  Barros. Abstract Simulators for the DSDE Formalism. 1998.
  https://repository.lib.ncsu.edu/bitstream/handle/1840.4/6989/1998_0056.pdf"
  [executive-name executive-model]
  {:executive-name  executive-name
   :executive-model executive-model})

(defn network-model? [model]
  (= ::network (:type model)))
