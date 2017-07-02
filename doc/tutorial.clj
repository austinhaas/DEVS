(ns tutorial
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.core.async :as async :refer [<! chan close! go put!]]
   [pettomato.devs.util :refer [infinity]]
   [pettomato.devs.atomic-simulator :refer [atomic-simulator]]
   [pettomato.devs.network-simulator :refer [network-simulator]]
   [pettomato.devs.immediate-system :refer [immediate-system]]
   [pettomato.devs.real-time-system :refer [real-time-system]]
   [pettomato.devs.models :refer [atomic-model executive-model network-model register unregister connect disconnect]]))

;; This is a tutorial walk-through of this DEVS implementation.

;; Comments preceded by [I] pertain to this implementation, and not
;; DEVS generally.

;; There have been many extensions to the original DEVS
;; formalism. This version is Dynamic Parallel DEVS with Ports.

;; Evaluate each expression in this file interactively.

;;; Example 1: An atomic model representing a simple timer.

;; The model is initially passive. Upon receiving a message, the model
;; transitions into the active state. In the active state, after
;; 'duration' milliseconds, the model outputs "Ding!" and then returns
;; to the passive state with the timer reset. If a message is received
;; while in the active state, the model will return to the passive
;; state, and the timer will be reset.

(defn timer-1 [duration]
  (atomic-model
   'passive
   (fn int-update [s]     'passive)
   (fn ext-update [s e x] (case s
                            passive 'active
                            active  'passive))
   nil
   (fn output     [s]     "Ding!")
   (fn ta         [s]     (case s
                            passive infinity
                            active  duration))))

;; The model can be run by passing it to a simulator and [I] the
;; simulator is passed to a system.

;; Here we are creating a timer with a 3000 millisecond duration.

;; The system is run from 0 to 10000 milliseconds of simulation time.

(-> (timer-1 3000)
    atomic-simulator
    (immediate-system 0 10000))

;; => []

;; There is no output, because no messages were sent to the timer.

;; Here it is again, but after 1 second an empty message is sent to
;; the timer.

(-> (timer-1 3000)
    atomic-simulator
    (immediate-system 0 10000 [[1000 nil]]))

;; => [[4000 "Ding!"]]

;; After 3 seconds (of simulation time), "Ding!" was output.

;; Note that "nil" has no special significance here. The model doesn't
;; check the input, so we could've used any value.

;; [I] The immediate-system runs the simulation as fast as
;; possible. The simulation can be run in real-time using the
;; real-time-system.

;; The real-time-system employs core.async channels to transfer input
;; and output.

;; This helper function will read and print output.

(defn listen! [prompt ch]
  (println "Listening...")
  (go (loop []
        (if-let [v (<! ch)]
          (do (println prompt v)
              (recur))
          (println "Listening done.")))))

(def chan-in  (chan 10))
(def chan-out (chan 10))

(listen! "> " chan-out)

(-> (timer-1 3000)
    atomic-simulator
    (real-time-system 0 chan-in chan-out))

(put! chan-in [nil])

;; Wait for 3 seconds. You should see something like "> [[6147
;; Ding!]]" printed. The number indicates when, in simulation time,
;; the output occurred.

;; You can evaluate that expression again to restart the timer.

;; Try evaluating it again before it returns output, to cancel the
;; timer.

(close! chan-in)
(close! chan-out)

;; Here is the same timer with comments to explain the components.

(defn timer-1 [duration]

  (atomic-model

   ;; This is the initial state. It can be anything. It is opaque
   ;; outside this model (except in the case of "executive" models,
   ;; described below). The remaining components are all functions that
   ;; take the current state as input.

   'passive

   ;; The next three functions are state transition functions. They
   ;; take a state and return a new state.

   ;; First, the internal state transition function. It is called at a
   ;; time determined by the behavior of the model. A model that is
   ;; ready for an internal state transition is called imminent.

   (fn int-update [s]     'passive)

   ;; Second, the external state transition function. It is called
   ;; when new messages are applicable to this model. It takes an
   ;; initial state, the elapsed time since the last state transition,
   ;; and a bag of input messages. (A bag is a set that allows
   ;; duplicates, or an unordered list.) For simplicity, we will be
   ;; ignoring the fact that the messages are supposed to be a bag for
   ;; now.

   (fn ext-update [s e x] (case s
                            passive 'active
                            active  'passive))

   ;; Third, the confluent state transition function. It is called
   ;; when a model is both imminent and has pending external
   ;; messages. The default, which is used when a nil value is
   ;; supplied, is to call the internal state transition function and
   ;; then immediately call the external state transition function.

   nil

   ;; Next, the output function. It takes the current state and
   ;; returns a bag of output messages. Here, again, for simplicity,
   ;; we are ignoring the bag for now.

   (fn output     [s]     "Ding!")

   ;; Finally, the time-advance function. It takes the current state
   ;; and returns the amount of time remaining until the next internal
   ;; state transition.

   (fn ta         [s]     (case s
                            passive infinity
                            active  duration))))

;; The user never calls any of the model functions directly. Each
;; model is paired with a simulator that "runs" it.

;; The simulator implements the following semantics:

;;   The time-advance function is called immediately _after_ any state
;;   transition function.

;;   The output function is called immediately _before_ the internal
;;   or confluent state transition functions. In other words, it is
;;   called when the model is imminent, immediately before the state
;;   transition function.

;; Note that the state transition functions do not produce any output
;; directly. Also, the output function and time-advance function
;; cannot change the state.

;; Why is it setup like this? For one thing, it maintains a causal
;; order. When there is a network of models, messages will propagate
;; in a breadth-first order, rather than a depth-first order, which
;; would happen if the external state transition function was able to
;; output messages. That also allows all imminent models to be run in
;; parallel.

;;; Example 2: A timer that takes duration as an input value.

;; The input value is stored in the model's state component.

(def timer-2
  (atomic-model
   {:phase 'passive
    :sigma infinity}
   (fn int-update [s]
     {:phase 'passive :sigma infinity})
   (fn ext-update [s e x]
     (case (:phase s)
       passive {:phase 'active  :sigma x}
       active  {:phase 'passive :sigma infinity}))
   nil
   (fn output [s] "Ding!")
   :sigma))

(-> timer-2
    atomic-simulator
    (immediate-system 0 10000 [[1000 2000]
                               [5000 1333]]))

;; => [[3000 "Ding!"] [6333 "Ding!"]]

;; Next, the input is sent to the timer once to start and once more at
;; the exact time it should ding. What do you think will happen?

(-> timer-2
    atomic-simulator
    (immediate-system 0 10000 [[1000 2000]
                               [3000 1333]]))

;; => [[3000 "Ding!"] [4333 "Ding!"]]

;; The internal state transition was processed before the external
;; state transition, because that is the default.

;; We can change that.

;; Example 3: A timer that prioritizes external state transitions over
;; internal state transitions.

(def timer-3
  ;; These functions are bound to symbols so that they can be called
  ;; in the confluent state transition function below.
  (let [int-update (fn int-update [s]
                     {:phase 'passive :sigma infinity})
        ext-update (fn ext-update [s e x]
                     (case (:phase s)
                       passive {:phase 'active  :sigma x}
                       active  {:phase 'passive :sigma infinity}))]
   (atomic-model
    {:phase 'passive
     :sigma infinity}
    int-update
    ext-update
    (fn con-update [s e x]
      ;; (ext-update (int-update s) 0 x) <-- This is the default.
      (int-update (ext-update s e x)))
    (fn output [s] "Ding!")
    :sigma)))

(-> timer-3
    atomic-simulator
    (immediate-system 0 10000 [[1000 2000]
                               [3000 1333]]))

;; => [[3000 "Ding!"]]

;; The timer still dings, because the output function is called when a
;; model is imminent. The second timer is added (because the external
;; event is processed first) and then immediately cancelled when the
;; internal update occurs.

;; Example 4: To suppress the ding completely, another phase is
;; required.

(def timer-4
  (let [int-update (fn int-update [s]
                     (case (:phase s)
                       passive s
                       active  {:phase 'ding    :sigma 0}
                       ding    {:phase 'passive :sigma infinity}))
        ext-update (fn ext-update [s e x]
                     (case (:phase s)
                       passive {:phase 'active  :sigma x}
                       active  {:phase 'passive :sigma infinity}))]
   (atomic-model
    {:phase 'passive
     :sigma infinity}
    int-update
    ext-update
    (fn con-update [s e x] (int-update (ext-update s e x)))
    (fn output [s]
      (case (:phase s)
        active nil
        ding   "Ding!"))
    :sigma)))

(-> timer-4
    atomic-simulator
    (immediate-system 0 10000 [[1000 2000]
                               [3000 1333]]))

;; => []

;; Example 5: A timer that cannot be interrupted.

(defn timer-5 [duration]
  (atomic-model
   {:phase 'passive
    :sigma infinity}
   (fn int-update [s]
     {:phase 'passive :sigma infinity})
   (fn ext-update [s e x]
     (case (:phase s)
       passive {:phase 'active :sigma duration}
       active  {:phase 'active :sigma (- (:sigma s) e)}))
   nil
   (fn output [s] "Ding!")
   :sigma))

(-> (timer-5 3000)
    atomic-simulator
    (immediate-system 0 10000 [[1000 nil]
                               [2000 nil]]))

;; => [[4000 "Ding!"]]

;; Notice that in the external state transition function, sigma was
;; reduced by e, the amount of time since the last state
;; transition. That is because the time-advance function will be
;; queried again immediately after this function returns. If sigma
;; wasn't reduced, then the duration would be reset to its original
;; value each time a message was received.

;; In my experience, `e` can be ignored about half the time.

;; Most often, you'll want to keep track of sigma, which represents
;; the time remaining. Occassionally, it is more helpful to add `e` to
;; the cumulative time since some event occurred.

;;; Bags of messages and ports.

;; In the examples above, we ignored the fact that messages are
;; collected into bags (i.e., unordered collections). The code above
;; works because atomic-simulator just passes along whatever is sent
;; to it. That's not the case for coupled/networked models, which will
;; be explained after this section. So, let's get that straight now.

;; In "DEVS with Ports," a message is a port and a value. A port can
;; be anything. It's just a label attached to a value. The idea is
;; that a model can accept values on different ports. When we start
;; connecting models, we'll see that models are connected via their
;; ports.

;; So far, we've only seen models that take a single type of value as
;; input.

;; Example 6: A timer with two inputs. One sets the duration. The
;; other toggles the countdown.

;; [I] Bags of messages are implemented as a map from a port to a
;; sequence of values.

(defn timer-6 [duration]
  (atomic-model
   {:phase    'passive
    :duration duration
    :sigma    infinity}
   (fn int-update [s]
     {:phase 'passive :duration (:duration s) :sigma infinity})
   (fn ext-update [s e x]
     (as-> s s
       (update s :sigma - e)
       (reduce (fn [s v] (assoc s :duration v))
               s
               (:set-time x))
       (reduce (fn [s _]
                 (case (:phase s)
                   passive (assoc s :phase 'active  :sigma (:duration s))
                   active  (assoc s :phase 'passive :sigma infinity)))
               s
               (:toggle x))))
   nil
   (fn output [s] {:out ["Ding!"]})
   :sigma))

(-> (timer-6 1000)
    atomic-simulator
    (immediate-system 0 10000 [[1000 {:set-time [2000]}]
                               [2000 {:toggle   [nil]}]
                               [3000 {:set-time [1234]}]
                               [4000 {:toggle   [nil]}]]))

;; This example doesn't illustrate the value of multiple incoming
;; values arriving on the same port, but you can imagine a model that
;; takes a collection of jobs to process.

;;; Coupled models.

;; Models can be combined to form coupled models. A coupled model is a
;; declarative specification of a set of component models and a
;; set of connections between them.

;; Hypothetical example:

#_
(def a-coupled-model
  {:components  {'foo (timer-1 1000)
                 'bar another-atomic-model
                 'baz a-coupled-model}
   :connections [['foo 'foo-port-out 'bar 'bar-port-in]
                 ['bar 'bar-port-out 'baz 'baz-port-in]]})

;; Models are labeled (foo, bar, baz in this example), and connections
;; are from model-label/port-label to model-label/port-label.

;; No concrete examples are provided because we are using a different
;; system called Dynamic DEVS which subsumes this behavior, but is
;; slightly more complicated.

;; DEVS is "closed under coupling." That means that a coupled model
;; has the same external semantics as an atomic model. Coupled models
;; can be composed of any combination of atomic and coupled models,
;; creating a hierarchy of models.

;;; Dynamic DEVS. Network. Exec. :N

;; The Dynamic DEVS formalism implements composition slightly
;; differently. Coupled models, referred to as networks, contain a
;; designated "executive" model. The executive model is an atomic
;; model like any other atomic model, except that its state contains
;; the network structure: the components and connections. (Unlike
;; other atomic models, the executive's state is not a black box at
;; this time.) Since the network structure is contained in the
;; executive's state, the executive can change the network structure
;; as easily as changing state.

;; The simulator processes executives after all non-executive
;; models. This ensures that there aren't any straggler messages when
;; the executive changes state.

;; Here's an example:

;; First, we create an atomic delay model to build upon. This model
;; takes an input value on an :in port and returns that value on an
;; :out port after processing-time.

(defn delay [processing-time]
  (atomic-model
   {:phase 'passive
    :sigma infinity
    :store nil}
   (fn int-update [s]
     (assoc s :phase 'passive :sigma infinity))
   (fn ext-update [s e x]
     (case (:phase s)
       passive (assoc s
                      :phase 'busy
                      :sigma processing-time
                      :store (first (:in x)))
       busy    (update s :sigma - e)))
   nil
   (fn output [s]
     {:out [(:store s)]})
   :sigma))

(-> (delay 1000)
    atomic-simulator
    (immediate-system 0 10000 [[1000 {:in ["Apple"]}]]))

;; => [[2000 {:out ["Apple"]}]]

;; Here is the executive model.

(def exec-1
  (executive-model
   (-> {}
       (register 'delay-1 (delay 1000))
       (register 'delay-2 (delay 500))
       (connect :N       :in  'delay-1 :in)
       (connect 'delay-1 :out 'delay-2 :in)
       (connect 'delay-2 :out :N       :out))
   nil nil nil nil
   (constantly infinity)))

;; `register` and `connect` are convenience functions that add the
;; appropriate network structure data to the state.

;; :N stands for the network that is the parent model to this
;; executive and any models the executive contains in the network
;; structure in its state.

;; This executive model has no behavior. The state transition
;; functions and the output function are not defined because they will
;; never be called. It only serves to supply the network structure.

;; Now we will create a network, with exec-1 as the network executive.

(def network-1 (network-model :exec exec-1))

(-> network-1
    network-simulator
    (immediate-system 0 10000 [[1000 {:in ["Banana"]}]]))

;; => [[2500 {:out ("Banana")}]]

;; There can be multiple connections to/from a single port. For
;; instance, you could add another delay, delay-3, to the network
;; above that runs parallel to delay-2, with the same inputs and
;; outputs. delay-1 would send each output to both delay-2 and
;; delay-3, and delay-2 and delay-3 would both send output to :N. You
;; cannot, however, have more than one connection between the same two
;; ports.

;;; Finally, here's an example of a dynamic network.

;; This network executive monitors a stream of input jobs and creates
;; new "servers", represented as delay models, to meet the demand.

(defn exec-2 [id threshold]
  (let [add-server (fn [s]
                     (let [sid (gensym)]
                       (-> s
                           (register sid (delay 1000))
                           (connect id  [:out sid] sid :in)
                           (connect sid :out       id  [:in sid])
                           (update :idle conj sid))))
        rem-server (fn [s]
                     (let [sid (peek (:idle s))]
                       (-> s
                           (unregister sid)
                           (disconnect id  [:out sid] sid :in)
                           (disconnect sid :out       id  [:in sid])
                           (update :idle pop))))
        ingest     (fn [s x]
                     (reduce-kv
                      (fn [s port v*]
                        (case port
                          :in (update s :job-queue into v*)
                          (let [[_ sid] port]
                            (-> s
                                (update-in [:output :out] conj ['done (first v*)])
                                (update :idle conj sid)))))
                      s
                      x))
        maybe-add  (fn [s]
                     (if (> (- (count (:job-queue s)) threshold)
                            (count (:idle s)))
                       (recur (add-server s))
                       s))
        maybe-rem  (fn [s]
                     (if (< (+ (count (:job-queue s)) threshold)
                            (count (:idle s)))
                       (recur (rem-server s))
                       s))
        process    (fn [s]
                     (if (and (seq (:idle      s))
                              (seq (:job-queue s)))
                       (let [sid (peek (:idle      s))
                             job (peek (:job-queue s))]
                         (-> s
                             (update :idle      pop)
                             (update :job-queue pop)
                             (update-in [:output [:out sid]] conj job)
                             recur))
                       s))]
    (executive-model
     (-> {:idle      []
          :job-queue []
          :output    {}
          :sigma     infinity}
         (connect :N :in  id :in)
         (connect id :out :N :out)
         add-server)
     (fn int-update [s]
       (assoc s :output {} :sigma infinity))
     (fn ext-update [s e x]
       (-> s
           (ingest x)
           maybe-add
           maybe-rem
           process
           (assoc :sigma 0)))
     nil
     :output
     :sigma)))

(def network-2 (network-model :exec (exec-2 :exec 5)))

(-> network-2
    network-simulator
    (immediate-system 0 100000 [[0 {:in (range 10)}]])
    pprint)

;; Note that the ports connecting the executive to the servers have
;; the form [:out eid], rather than something like :out-n. This is
;; called a labeled port. Remember, ports can be anything. Labeled
;; ports are an easy way to implement dynamic port creation.

;;; Port transducers.

;; The connect function accepts an optional 5th parameter: a
;; transducer. The transducer will be applied to all values passing
;; through the connection.

;; Example coming soon.
