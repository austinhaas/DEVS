(ns pettomato.devs.examples.circuit
  (:require
   [pettomato.devs :refer [atomic-model network-model trace]]
   [pettomato.devs.util :refer [infinity]]))

;; Problem: How to implement these in a way such that their output is intuitive.

;; (The real problem is just getting some practice.)

;;------------------------------------------------------------------------------
;; Proposal #1

;; This version purports to be physically accurate. There will be an initial
;; period of "settling".

;; 1. The only message values are 1 or 0.

;; 2. Models send an initial value on each output port. (This wouldn't work for
;; a dynamic network.)

;; 3. Models only send a value if it differs from the last value sent on that
;; port.

;; This version doesn't propagate values unless they change.

;; If an input changes during the delay, and it does NOT affect the output, then
;; it wont have any affect on the delay clock.

;; If an input changes during the delay, and it does affect the output, then the
;; delay clock will be reset.

(defn inverter [delay]
  (atomic-model
   [{:out   false
     :sigma 0}
    0]
   (fn int-update [s]
     (assoc s :sigma infinity))
   (fn ext-update [s e x]
     (let [out (not (last (:in x)))]
       (if (= out (:out s))
         (update s :sigma - e)
         (assoc s :out out :sigma delay))))
   nil
   (fn output [s] {:out [(:out s)]})
   :sigma))

(defn and-gate [delay]
  (atomic-model
   [{:in-1  false
     :in-2  false
     :out   false
     :sigma 0}
    0]
   (fn int-update [s]
     (assoc s :sigma infinity))
   (fn ext-update [s e x]
     (let [;; Intake messages.
           s' (reduce-kv (fn [s port vs]
                           (case port
                             :in-1 (assoc s :in-1 (last vs))
                             :in-2 (assoc s :in-2 (last vs))))
                         s
                         x)
           ;; Compute result.
           s' (assoc s' :out (and (:in-1 s')
                                  (:in-2 s')))]
       ;; Update delay clock.
       (if (= (:out s) (:out s')) ;; Has the output changed?
         (update s' :sigma - e)
         (assoc s' :sigma delay))))
   nil
   (fn output [s] {:out [(:out s)]})
   :sigma))

(defn or-gate [delay]
  (atomic-model
   [{:in-1  false
     :in-2  false
     :out   false
     :sigma 0}
    0]
   (fn int-update [s]
     (assoc s :sigma infinity))
   (fn ext-update [s e x]
     (let [;; Intake messages.
           s' (reduce-kv (fn [s port vs]
                           (case port
                             :in-1 (assoc s :in-1 (last vs))
                             :in-2 (assoc s :in-2 (last vs))))
                         s
                         x)
           ;; Compute result.
           s' (assoc s' :out (or (:in-1 s')
                                 (:in-2 s')))]
       ;; Update delay clock.
       (if (= (:out s) (:out s')) ;; Has the output changed?
         (update s' :sigma - e)
         (assoc s' :sigma delay))))
   nil
   (fn output [s] {:out [(:out s)]})
   :sigma))

;;------------------------------------------------------------------------------

(defn half-adder
  "S will become 1 whenever precisely one of A and B is 1, and C will become 1
  whenever A and B are both 1. - SICP, p. 274"
  [inverter-delay and-gate-delay or-gate-delay]
  (network-model
   {:or    (or-gate  or-gate-delay)
    :and-1 (and-gate and-gate-delay)
    :and-2 (and-gate and-gate-delay)
    :inv   (inverter inverter-delay)}
   [[:network :a   :or      :in-1 identity]
    [:network :a   :and-1   :in-1 identity]
    [:network :b   :or      :in-2 identity]
    [:network :b   :and-1   :in-2 identity]
    [:or      :out :and-2   :in-1 identity]
    [:and-1   :out :inv     :in   identity]
    [:and-1   :out :network :c    identity]
    [:inv     :out :and-2   :in-2 identity]
    [:and-2   :out :network :s    identity]]))

(defn full-adder [inverter-delay and-gate-delay or-gate-delay]
  (network-model
   {:ha-1 (half-adder inverter-delay and-gate-delay or-gate-delay)
    :ha-2 (half-adder inverter-delay and-gate-delay or-gate-delay)
    :or   (or-gate or-gate-delay)}
   [[:network :a       :ha-1 :a identity]
    [:network :b       :ha-2 :a identity]
    [:network :c-in    :ha-2 :b identity]
    [:ha-1 :s :network :s       identity]
    [:ha-1 :c :or      :in-1    identity]
    [:ha-2 :s :ha-1    :in-2    identity]
    [:ha-2 :c :or      :in-2    identity]
    [:or   :c :network :c       identity]]))
