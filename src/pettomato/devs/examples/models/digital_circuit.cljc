(ns pettomato.devs.examples.models.digital-circuit
  "This is loosely based on SICP, Ch. 3.3.4."
  (:require
   [pettomato.devs.examples.models :as m]
   [pettomato.devs.lib.hyperreal :as h]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.models.atomic-model :refer [def-atomic-model
                                               internal-update
                                               external-update
                                               output
                                               time-advance]]
   [pettomato.devs.root-coordinators.afap-root-coordinator :refer [afap-root-coordinator]]
   [pettomato.devs.simulators.network-simulator :refer [network-simulator]]))

;; This is supposed to be physically accurate in the sense that it takes time
;; for signals to propagate. There will be an initial period of "settling".

;; 1. The only message values are true or false, indicating signal or no-signal
;; respectively.

;; 2. Models send an initial value on each output port. (This wouldn't work for
;; a dynamic network.)

;; 3. Models only send a value if it differs from the last value sent on that
;; port.

;; If an input changes during the delay, then the delay timer will be reset.

;;------------------------------------------------------------------------------
;; Primitive function boxes

(def-atomic-model Inverter [delay sigma has-power? input output last-output]
  (internal-update [state]
    (assoc state
           :sigma h/infinity
           :last-output output))
  (external-update [state elapsed mail]
    (let [has-power? (if (contains? mail :pwr) (rand-nth (:pwr mail)) has-power?)
          input      (if (contains? mail :in)  (rand-nth (:in  mail)) input)
          output     (not input)]
      (assoc state
             :sigma      delay
             :has-power? has-power?
             :input      input
             :output     output)))
  (output [state]
    (if (and has-power?
             (not= output last-output))
      {:out [output]}
      {}))
  (time-advance [state]
    (if has-power?
      sigma
      h/infinity)))

(defn inverter [delay]
  (assert (h/hyperreal? delay))
  (->Inverter delay h/infinity false false nil nil))

(def-atomic-model AndGate [delay sigma has-power? input-1 input-2 output last-output]
  (internal-update [state]
    (assoc state
           :sigma h/infinity
           :last-output output))
  (external-update [state elapsed mail]
    (let [has-power? (if (contains? mail :pwr)  (rand-nth (:pwr  mail)) has-power?)
          input-1    (if (contains? mail :in-1) (rand-nth (:in-1 mail)) input-1)
          input-2    (if (contains? mail :in-2) (rand-nth (:in-2 mail)) input-2)
          output     (and input-1 input-2)]
      (assoc state
             :sigma      delay
             :has-power? has-power?
             :input-1    input-1
             :input-2    input-2
             :output     output)))
  (output [state]
    (if (and has-power?
             (not= output last-output))
      {:out [output]}
      {}))
  (time-advance [state]
    (if has-power?
      sigma
      h/infinity)))

(defn and-gate [delay]
  (assert (h/hyperreal? delay))
  (->AndGate delay h/infinity false false false nil nil))

(def-atomic-model OrGate [delay sigma has-power? input-1 input-2 output last-output]
  (internal-update [state]
    (assoc state
           :sigma h/infinity
           :last-output output))
  (external-update [state elapsed mail]
    (let [has-power? (if (contains? mail :pwr)  (rand-nth (:pwr  mail)) has-power?)
          input-1    (if (contains? mail :in-1) (rand-nth (:in-1 mail)) input-1)
          input-2    (if (contains? mail :in-2) (rand-nth (:in-2 mail)) input-2)
          output     (or input-1 input-2)]
      (assoc state
             :sigma      delay
             :has-power? has-power?
             :input-1    input-1
             :input-2    input-2
             :output     output)))
  (output [state]
    (if (and has-power?
             (not= output last-output))
      {:out [output]}
      {}))
  (time-advance [state]
    (if has-power?
      sigma
      h/infinity)))

(defn or-gate [delay]
  (assert (h/hyperreal? delay))
  (->OrGate delay h/infinity false false false nil nil))

;;------------------------------------------------------------------------------
;; Composite function boxes

(defn half-adder
  "S will become 1 whenever precisely one of A and B is 1, and C will become 1
  whenever A and B are both 1. - SICP, p. 274"
  [inverter-delay and-gate-delay or-gate-delay]
  (m/simple-network-model
   :exec
   {:or    [(or-gate  or-gate-delay)  h/zero]
    :and-1 [(and-gate and-gate-delay) h/zero]
    :and-2 [(and-gate and-gate-delay) h/zero]
    :inv   [(inverter inverter-delay) h/zero]}
   [[:network :pwr :or :pwr]
    [:network :pwr :and-1 :pwr]
    [:network :pwr :and-2 :pwr]
    [:network :pwr :inv :pwr]
    [:network :a   :or      :in-1]
    [:network :a   :and-1   :in-1]
    [:network :b   :or      :in-2]
    [:network :b   :and-1   :in-2]
    [:or      :out :and-2   :in-1]
    [:and-1   :out :inv     :in  ]
    [:and-1   :out :network :c   ]
    [:inv     :out :and-2   :in-2]
    [:and-2   :out :network :s   ]]))

(defn full-adder [inverter-delay and-gate-delay or-gate-delay]
  (m/simple-network-model
   :exec
   {:ha-1 [(half-adder inverter-delay and-gate-delay or-gate-delay)
           h/zero]
    :ha-2 [(half-adder inverter-delay and-gate-delay or-gate-delay)
           h/zero]
    :or   [(or-gate or-gate-delay)
           h/zero]}
   [[:network :pwr     :ha-1 :pwr]
    [:network :pwr     :ha-2 :pwr]
    [:network :pwr     :or   :pwr]
    [:network :a       :ha-1 :a]
    [:network :b       :ha-2 :a]
    [:network :c-in    :ha-2 :b]
    [:ha-1 :s :network :s      ]
    [:ha-1 :c :or      :in-1   ]
    [:ha-2 :s :ha-1    :b      ]
    [:ha-2 :c :or      :in-2   ]
    [:or :out :network :c      ]]))

;;------------------------------------------------------------------------------
;; Ripple carry adder

(defn ripple-carry-adder
  "SICP, p. 278

  I'm handling indexes differently from SICP. I'm treating 0 as the index of the
  least significant bit, while SICP appears to be using 1 as the index of the
  most significant bit."
  [n-bits inverter-delay and-gate-delay or-gate-delay]
  (let [key-fn   #(keyword (str "fa-" %))
        models   (into {} (for [i (range n-bits)]
                            [(key-fn i) [(full-adder inverter-delay and-gate-delay or-gate-delay)
                                         h/zero]]))
        ;; Connect them to the network. We don't connect an external carry
        ;; input.
        routes-1 (apply concat
                        [[(key-fn (dec n-bits)) :c :network :c]]
                        (for [i (range n-bits)]
                          [[:network :pwr (key-fn i) :pwr]
                           [:network [:a i] (key-fn i) :a]
                           [:network [:b i] (key-fn i) :b]
                           [(key-fn i) :s :network [:s i]]]))
        ;; Connect them to each other.
        routes-2 (map (fn [i j]
                        [(key-fn i) :c (key-fn j) :c-in])
                      (range n-bits)
                      (range 1 n-bits))
        routes   (concat routes-1 routes-2)]
    (m/simple-network-model :exec models routes)))

(defn encode
  "Converts an number into mail."
  [n-bits n port-label]
  (into {} (for [i (range n-bits)]
             (let [v (bit-test n i)]
               [[port-label i] [v]]))))

(defn decode
  "Converts mail into a number."
  [xs]
  (reduce (fn [n [i b]]
            (if b
              (bit-set   n i)
              (bit-clear n i)))
          0
          (for [[t m]  xs
                [k vs] m
                :when (and (vector? k) (= :s (first k)))
                :let  [[k i] k]
                v      vs]
            [i v])))

(defn ripple-carry-add
  "Adds two numbers using a ripple adder."
  [n-bits a b & {:keys [inverter-delay and-gate-delay or-gate-delay]
                 :or   {inverter-delay (h/*R 0 2)
                        and-gate-delay (h/*R 0 3)
                        or-gate-delay  (h/*R 0 5)}}]
  (-> (m/simple-network-model
       :exec
       {:gen [(m/generator [[(h/*R 1) {:pwr [true]}]
                            [(h/*R 1000) (merge (encode n-bits a :a)
                                                (encode n-bits b :b))]])
              (h/*R 1)]
        :rca [(ripple-carry-adder n-bits
                                  inverter-delay
                                  and-gate-delay
                                  or-gate-delay)
              h/zero]}
       (apply concat
              [[:gen :pwr :rca :pwr]
               [:rca :c :network :c]]
              (for [i (range n-bits)]
                [[:gen [:a i] :rca [:a i]]
                 [:gen [:b i] :rca [:b i]]
                 [:rca [:s i] :network [:s i]]])))
      network-simulator
      afap-root-coordinator
      decode))
