(ns pettomato.devs.simulators.atomic-simulator
  "A simulator for atomic models."
  (:require
   #?(:cljs [goog.string :as gstring :refer [format]])
   #?(:cljs [goog.string.format])
   [pettomato.devs.models.atomic-model :refer [internal-update
                                               external-update
                                               confluent-update
                                               output
                                               time-advance
                                               atomic-model?]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [Simulator]]))

(defrecord AtomicSimulator [initial-state initial-elapsed state tl tn]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    (assert (h/<= h/zero initial-elapsed) "initial-elapsed must be <= 0")
    (let [tl (h/- t initial-elapsed)
          ta (time-advance initial-state)
          tn (h/+ tl ta)]
         (assert (h/< tl tn) "tl must be < tn")
      (assoc sim :state initial-state :tl tl :tn tn)))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (assert (h/= t tn) (str "synchronization error: (not (= " t " " tn "))"))
    (output state))
  (transition [sim mail t]
    (log/tracef "--- transition ---")
    (assert (h/<= tl t tn) (str "synchronization error: (not (<= " tl " " t " " tn "))"))
    (let [state (cond
                  (and (h/= t tn) (empty? mail)) (internal-update state)
                  (and (h/= t tn) (seq    mail)) (confluent-update state mail)
                  (and (h/< t tn) (seq    mail)) (external-update state (h/- t tl) mail)
                  :else                          (throw (ex-info "Illegal state for transition; sim is not imminent nor receiving mail."
                                                                 {:tl         tl
                                                                  :t          t
                                                                  :tn         tn
                                                                  :mail-count (count mail)})))
          ta    (time-advance state)
          tn    (h/+ t h/epsilon ta)]
         (assoc sim :state state :tl t :tn tn)))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(defn format-atomic-simulator [x]
  #?(:clj  (format "#pettomato.devs.simulators.AtomicSimulator<0x%x>{}"
                   (System/identityHashCode x))
     :cljs (format "#pettomato.devs.simulators.AtomicSimulator<%s>{}"
                   (.toString (hash x) 16))))

#?(:clj
   (defn print-atomic-simulator [x w]
     (.write w (format-atomic-simulator x))))

#?(:clj
   (defmethod print-method AtomicSimulator [x w]
     (print-atomic-simulator x w)))

#?(:clj
   (. clojure.pprint/simple-dispatch
      addMethod
      AtomicSimulator
      #(print-atomic-simulator % *out*)))

(defn atomic-simulator
  "Wrap an atomic model in an atomic simulator.

  Args:
    model - An atomic model.

  Returns:
    A simulator."
  [model & {:keys [elapsed]
            :or   {elapsed h/zero}}]
  (assert (atomic-model? model))
  (map->AtomicSimulator {:initial-state   model
                         :initial-elapsed elapsed}))
