(ns pettomato.devs.examples.circuit
  (:require
   [pettomato.devs :refer [atomic-model network-model]]
   [pettomato.devs.util :refer [infinity]]))

(defn inverter [delay]
  (atomic-model
   [{:in    false
     :sigma infinity}
    0]
   (fn int-update [s]
     (assoc s :sigma infinity))
   (fn ext-update [s e x]
     (let [s (reduce #(assoc %1 :in (not %2)) s (:in x))]
       (assoc s :sigma delay)))
   nil
   (fn [s] {:out [(:in s)]})
   :sigma))

(defn and-gate [delay]
  (atomic-model
   [{:in-1  false
     :in-2  false
     :sigma infinity}
    0]
   (fn int-update [s]
     (assoc s :sigma infinity))
   (fn ext-update [s e x]
     (let [s (reduce #(assoc %1 :in-1 %2) s (:in-1 x))
           s (reduce #(assoc %1 :in-2 %2) s (:in-2 x))]
       ;; If another input is received before this model is imminent, the
       ;; countdown (sigma) will be reset.

       ;; To work around that, we could add a queue.

       (assoc s :sigma delay)))
   nil
   (fn output [s] {:out [(and (:in-1 s) (:in-2 s))]})
   :sigma))

(defn or-gate [delay]
  (atomic-model
   [{:in-1  false
     :in-2  false
     :sigma infinity}
    0]
   (fn int-update [s]
     (assoc s :sigma infinity))
   (fn ext-update [s e x]
     (let [s (reduce #(assoc %1 :in-1 %2) s (:in-1 x))
           s (reduce #(assoc %1 :in-2 %2) s (:in-2 x))]
       (assoc s :sigma delay)))
   nil
   (fn output [s] {:out [(or (:in-1 s) (:in-2 s))]})
   :sigma))

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
