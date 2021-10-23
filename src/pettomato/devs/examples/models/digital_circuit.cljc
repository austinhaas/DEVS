(ns pettomato.devs.examples.models.digital-circuit
  (:require
   [pettomato.devs.examples.models :refer [lazy-seq-generator]]
   [pettomato.devs.lib.hyperreal :as h :refer [*R]]
   [pettomato.devs.models.atomic-model :refer [atomic-model]]
   [pettomato.devs.models.network-model :refer [network-model]]
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

;; If an input changes during the delay, and it does NOT affect the output, then
;; it won't have any affect on the delay clock.

;; If an input changes during the delay, and it does affect the output, then the
;; delay clock will be reset.

;;------------------------------------------------------------------------------
;; Primitive function boxes

(defn inverter [delay]
  (assert (h/hyperreal? delay))
  (atomic-model
   :initial-state   {:out   false
                     :sigma h/epsilon}
   :initial-elapsed-time h/epsilon
   :internal-update (fn [s] (assoc s :sigma h/infinity))
   :external-update (fn [s e x]
                      (let [out (not (last (:in x)))]
                        (if (= out (:out s))
                          (update s :sigma h/- e)
                          (assoc s :out out :sigma delay))))
   :output          (fn [s] {:out [(:out s)]})
   :time-advance    :sigma))

(defn and-gate [delay]
  (assert (h/hyperreal? delay))
  (atomic-model
   :initial-state   {:in-1  false
                     :in-2  false
                     :out   false
                     :sigma h/epsilon}
   :initial-elapsed-time h/epsilon
   :internal-update (fn [s] (assoc s :sigma h/infinity))
   :external-update (fn [s e x]
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
                          (update s' :sigma h/- e)
                          (assoc s' :sigma delay))))
   :output          (fn [s] {:out [(:out s)]})
   :time-advance    :sigma))

(defn or-gate [delay]
  (assert (h/hyperreal? delay))
  (atomic-model
   :initial-state   {:in-1  false
                     :in-2  false
                     :out   false
                     :sigma h/epsilon}
   :initial-elapsed-time h/epsilon
   :internal-update (fn [s] (assoc s :sigma h/infinity))
   :external-update (fn [s e x]
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
                          (update s' :sigma h/- e)
                          (assoc s' :sigma delay))))
   :output          (fn [s] {:out [(:out s)]})
   :time-advance    :sigma))

;;------------------------------------------------------------------------------
;; Composite function boxes

(defn half-adder
  "S will become 1 whenever precisely one of A and B is 1, and C will become 1
  whenever A and B are both 1. - SICP, p. 274"
  [inverter-delay and-gate-delay or-gate-delay]
  (network-model
   {:or    (or-gate  or-gate-delay)
    :and-1 (and-gate and-gate-delay)
    :and-2 (and-gate and-gate-delay)
    :inv   (inverter inverter-delay)}
   [[:network :a   :or      :in-1]
    [:network :a   :and-1   :in-1]
    [:network :b   :or      :in-2]
    [:network :b   :and-1   :in-2]
    [:or      :out :and-2   :in-1]
    [:and-1   :out :inv     :in  ]
    [:and-1   :out :network :c   ]
    [:inv     :out :and-2   :in-2]
    [:and-2   :out :network :s   ]]))

(defn full-adder [inverter-delay and-gate-delay or-gate-delay]
  (network-model
   {:ha-1 (half-adder inverter-delay and-gate-delay or-gate-delay)
    :ha-2 (half-adder inverter-delay and-gate-delay or-gate-delay)
    :or   (or-gate or-gate-delay)}
   [[:network :a       :ha-1 :a]
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
                            [(key-fn i) (full-adder inverter-delay and-gate-delay or-gate-delay)]))
        ;; Connect them to the network. We don't connect an external carry
        ;; input.
        routes-1 (apply concat
                        [[(key-fn (dec n-bits)) :c :network :c]]
                        (for [i (range n-bits)]
                          [[:network [:a i] (key-fn i) :a]
                           [:network [:b i] (key-fn i) :b]
                           [(key-fn i) :s :network [:s i]]]))
        ;; Connect them to each other.
        routes-2 (map (fn [i j]
                        [(key-fn i) :c (key-fn j) :c-in])
                      (range n-bits)
                      (range 1 n-bits))
        routes   (concat routes-1 routes-2)]
    (network-model models routes)))

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
                 :or   {inverter-delay (*R 2)
                        and-gate-delay (*R 3)
                        or-gate-delay  (*R 5)}}]
  (-> (network-model {:gen (lazy-seq-generator [[(*R 1000) (merge (encode n-bits a :a)
                                                                  (encode n-bits b :b))]])
                      :rca (ripple-carry-adder n-bits
                                               inverter-delay
                                               and-gate-delay
                                               or-gate-delay)}
                     (apply concat
                            [[:rca :c :network :c]]
                            (for [i (range n-bits)]
                              [[:gen [:a i] :rca [:a i]]
                               [:gen [:b i] :rca [:b i]]
                               [:rca [:s i] :network [:s i]]])))
      network-simulator
      afap-root-coordinator
      decode))
