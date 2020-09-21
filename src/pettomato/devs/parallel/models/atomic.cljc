(ns pettomato.devs.parallel.models.atomic
  "An atomic model for Parallel DEVS with Ports.")

(defn atomic-model [init int ext con out ta]

  ;; init - [state elapsed-time] A "total" state.

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
