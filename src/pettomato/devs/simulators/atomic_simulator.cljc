(ns pettomato.devs.simulators.atomic-simulator
  "A simulator for atomic models."
  (:require
   [clojure.pprint]
   #?(:cljs [goog.string :as gstring :refer [format]])
   #?(:cljs [goog.string.format])
   [pettomato.devs.models.atomic-model :refer [internal-update
                                               external-update
                                               confluent-update
                                               output
                                               time-advance
                                               atomic-model?]]
   [pettomato.devs.lib.debug :refer [ex-assert]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [Simulator]])
  #?(:clj (:import [java.io Writer])))

(defrecord AtomicSimulator [initial-state initial-elapsed state tl tn]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    (ex-assert (h/<= h/zero initial-elapsed)
               "initial-elapsed must be <= 0"
               {:initial-elapsed initial-elapsed})
    (let [tl (h/- t initial-elapsed)
          tn (h/+ tl (time-advance initial-state))]
      (ex-assert (h/<= tl t)
                 "NIA violation"
                 {:tl tl :t t})
      (ex-assert (h/< t tn)
                 "NIA violation"
                 {:t t :tn tn})
      (assoc sim :state initial-state :tl tl :tn tn)))
  (collect-mail [sim t]
    (log/trace "--- collect-mail ---")
    (ex-assert (h/= t tn) "synchronization error" {:t t :tn tn})
    (output state))
  (transition [sim mail t]
    (log/tracef "--- transition ---")
    (ex-assert (h/<= tl t tn) "synchronization error"
               {:tl tl :t t :tn tn})
    (ex-assert (or (h/= t tn) (seq mail))
               "Illegal state for transition; sim is not imminent nor receiving mail."
               {:tl tl :t t :tn tn :mail-count (count mail)})
    (let [state (if (h/= t tn)
                  (if (seq mail)
                    (confluent-update state mail)
                    (internal-update state))
                  (external-update state (h/- t tl) mail))
          tl    t
          tn    (h/+ t (time-advance state))]
      (ex-assert (h/< tl tn)
                 "tn must be greater than tl."
                 {:tl tl :tn tn})
      (assoc sim :state state :tl tl :tn tn)))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn))

(defn format-atomic-simulator ^String [x]
  #?(:clj  (format "#pettomato.devs.simulators.AtomicSimulator<0x%x>{}"
                   (System/identityHashCode x))
     :cljs (format "#pettomato.devs.simulators.AtomicSimulator<%s>{}"
                   (.toString (hash x) 16))))

#?(:clj
   (defn print-atomic-simulator [^AtomicSimulator x ^java.io.Writer w]
     (.write w (format-atomic-simulator x))))

#?(:clj
   (defmethod print-method AtomicSimulator [^AtomicSimulator x ^java.io.Writer w]
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
  (ex-assert (atomic-model? model))
  (map->AtomicSimulator {:initial-state   model
                         :initial-elapsed elapsed}))
