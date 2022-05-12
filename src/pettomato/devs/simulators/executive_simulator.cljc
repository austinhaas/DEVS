(ns pettomato.devs.simulators.executive-simulator
  "A simulator for network executive models."
  (:require
   [clojure.pprint]
   #?(:cljs [goog.string :as gstring :refer [format]])
   #?(:cljs [goog.string.format])
   [pettomato.devs.models.atomic-model :refer [internal-update
                                               external-update
                                               confluent-update
                                               output
                                               time-advance]]
   [pettomato.devs.models.executive-model :refer [executive-model? structure-changes]]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.simulator :refer [Simulator]]))

(defprotocol IExecutiveSimulator
  (get-structure-changes [sim]))

(defrecord ExecutiveSimulator [initial-state initial-elapsed state tl tn]
  Simulator
  (initialize [sim t]
    (log/trace "--- initialize ---")
    (assert (h/<= h/zero initial-elapsed) "initial-elapsed must be <= 0")
    (let [tl (h/- t initial-elapsed)
          ta (time-advance initial-state)
          tn (h/+ tl ta)]
      (assert (h/pos? ta))
      (when (not (h/< tl tn))
        (throw (ex-info "tl must be < tn" {:tl tl :tn tn})))
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
          tn    (h/+ t ta)]
      (assoc sim :state state :tl t :tn tn)))
  (time-of-last-event [sim] tl)
  (time-of-next-event [sim] tn)
  IExecutiveSimulator
  (get-structure-changes [sim]
    (structure-changes state)))

(defn format-network-exec-simulator [x]
  #?(:clj  (format "#pettomato.devs.simulators.ExecutiveSimulator<0x%x>{}"
                   (System/identityHashCode x))
     :cljs (format "#pettomato.devs.simulators.ExecutiveSimulator<%s>{}"
                   (.toString (hash x) 16))))

#?(:clj
   (defn print-network-exec-simulator [x w]
     (.write w (format-network-exec-simulator x))))

#?(:clj
   (defmethod print-method ExecutiveSimulator [x w]
     (print-network-exec-simulator x w)))

#?(:clj
   (. clojure.pprint/simple-dispatch
      addMethod
      ExecutiveSimulator
      #(print-network-exec-simulator % *out*)))

(defn executive-simulator
  "Wrap a network executive model in a network executive simulator.

  Args:
    model - A network executive model.

  Returns:
    A simulator."
  [model & {:keys [elapsed]
            :or   {elapsed h/zero}}]
  (assert (executive-model? model))
  (map->ExecutiveSimulator {:initial-state   model
                            :initial-elapsed elapsed}))
