(ns tutorial
  (:require
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
;; formalism. This version of DEVS is Dynamic Parallel DEVS with
;; Ports.

;; Evaluate each expression in this file interactively.

;;; Example 1: An atomic model representing a simple timer.

;; The model is initially passive. Upon receiving a message, the model
;; transitions into the active state. In the active state, after
;; duration milliseconds, the model outputs "Ding!" and then returns
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

;; Here is the same thing, but after 1 second an empty message is sent
;; to the timer.

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

   ;; This is the initial state. It can be anything. It is opaque*
   ;; outside this model. The remaining components are all functions
   ;; that take the current state as input.

   'passive

   ;; The next three functions are state transition functions. They
   ;; take a state and return a new state.

   ;; First, is the internal state transition function. It is called
   ;; at a time determined by the behavior of the model. A model that
   ;; is ready for its internal state transition function is called
   ;; imminent.

   (fn int-update [s]     'passive)

   ;; Second, is the external state transition function. It is called
   ;; when new messages are applicable to this model. It takes an
   ;; initial state, the elapsed time since the last state transition,
   ;; and a bag of input messages. (A bag is a set that allows
   ;; duplicates, or an unordered list.) For simplicity, we will be
   ;; ignoring the fact that the messages are supposed to be a bag for
   ;; now.

   (fn ext-update [s e x] (case s
                            passive 'active
                            active  'passive))

   ;; Third, is the confluent state transition function. It is called
   ;; when a model is both imminent and due to receive messages at the
   ;; same time. The default, which is used when a nil value is
   ;; supplied, is to call the internal state transition function and
   ;; then immediately call the external state transition function.

   nil

   ;; Next, is the output function. It takes the current state and
   ;; returns a bag of output messages. Here, again, for simplicity,
   ;; we are ignoring the bag for now.

   (fn output     [s]     "Ding!")

   ;; Finally, is the time-advance function. It takes the current
   ;; state and returns the amount of time remaining until the next
   ;; internal state transition.

   (fn ta         [s]     (case s
                            passive infinity
                            active  duration))))

;; You never call any of the model functions directly. Each model is
;; paired with a simulator that "runs" it.

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

;; Example 2: A timer that takes duration as an input value.

;; We need to store the input value, so we'll expand state.

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

;; Here we send input to the timer once to start and once more at the
;; exact time it should ding. What do you think will happen?

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
  ;; Binding these functions to variables so we can call them in the
  ;; confluent state transition function below.
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

;; The timer still dings, because the output function is called
;; whenever the model is imminent.

;; Example 4: To suppress the ding completely, we can add another phase.

(def timer-4
  ;; Binding these functions to variables so we can call them in the
  ;; confluent state transition function below.
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
                               [2000 nil]
                               [3000 nil]]))

;; => [[4000 "Ding!"]]

;; Notice that in the external state transition function, sigma was
;; reduced by e, the amount of time since the last state
;; transition. That is because the time-advance function will be
;; queried again immediately after this function returns. If sigma
;; wasn't reduced, then the duration would be effectively reset to its
;; original value every time a message was received.

;; In my experience, about half the time you don't have to use e, but
;; half the time you do.

;; Most often, you'll want to keep track of sigma, the time remaining,
;; but occassionally it is more helpful to record the cumulative time
;; since some event by adding e to a counter.

;;; Bags of messages and ports.

;; In the examples above, I explained that we are ignoring the fact
;; that messages are collected into bags (i.e., unordered
;; collections). The code above works because atomic-simulator just
;; passes along whatever was sent without looking at it. That's not
;; the case for coupled/networked models, which will be explained
;; after this section. So, let's get that straight now.

;; In "DEVS with Ports," a message is a port and a value. A port can
;; be anything. It's just a label attached to a value. The idea is
;; that a model can accept values on different ports. When we start
;; connecting models, we'll see that we are connecting models via
;; their ports.

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
       (reduce (fn [s v]
                 (assoc s :duration v))
               s
               (:set-time x))
       (reduce (fn [s v]
                 (let [duration (:duration s)]
                   (case (:phase s)
                     passive {:phase 'active  :duration duration :sigma duration}
                     active  {:phase 'passive :duration duration :sigma infinity})))
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
                               [6000 {:toggle   [nil]}]]))

;; This example doesn't illustrate the value of multiple incoming
;; values arriving on the same port, but maybe you can imagine a model
;; that takes a collection of jobs to process.

;;; Coupled models.

;; Models can be combined to form coupled models. A coupled model has
;; no behavior. It contains a set of component models and a set of
;; connections between them.

;; Hypothetical example:

#_
(def a-coupled-model
  {:components  {'foo (timer-1 1000)
                 'bar another-atomic-model
                 'baz a-coupled-model}
   :connections [['foo 'foo-port-out 'bar 'bar-port-in]
                 ['bar 'bar-port-out 'baz 'baz-port-in]]})

;; So models are labeled (foo, bar, baz in this example), and
;; connections are from model-label/port-label to
;; model-label/port-label.

;; No concrete examples because we using a different system called
;; Dynamic DEVS which subsumes this behavior, but is slightly more
;; complicated.

;; Note that DEVS is closed under coupling. That means that a coupled
;; model has the same behavior as an atomic model. You can have
;; coupled models composed of coupled modes to create an elaborate
;; hierarchy of interacting models.

;;; Dynamic DEVS. Network. Exec. :N

;; The Dynamic DEVS formalism achieves composition slightly
;; differently. Coupled models, referred to as networks, contain a
;; designated "executive" model. The executive model is an atomic
;; model like any other atomic model, except that its state contains
;; the network structure: the components and connections.

;; *So, the executive's state is not a black box at this time.

;; Since the network structure is part of the state of the executive,
;; the executive can change the network structure whenever its state
;; changes. The simulator processes executives after all non-executive
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

;; register and connect are convenience functions to add the
;; appropriate network structure data to the state.

;; :N stands for the network, which is the parent model to this
;; executive and any models the executive contains in the network
;; structure in its state.

;; This model has no behavior. The state transition functions and the
;; output function are not defined because they will never be called.

;; Now we will create a network, with exec-1 as the network executive.

(def network-1 (network-model :exec exec-1))

(-> network-1
    network-simulator
    (immediate-system 0 10000 [[1000 {:in ["Banana"]}]]))

;; => [[2500 {:out ("Banana")}]]

;; Note that you can have multiple connections to/from a single
;; port. For instance, you could add another delay, delay-3, to the
;; network above, and send the output from delay-1 to both delay-2 and
;; the delay-3, and connect delay-3's out port to :N. You cannot,
;; however, have more than one connection between the same two ports.

;;; Finally, here's an example of a dynamic network.

;; This network executive monitors the incoming stream of input jobs
;; and creates new "servers" (i.e., new delay models) to meet the
;; demand.

(defn exec-2 [id threshold]
  (let [add-server (fn [s]
                     (let [sid (gensym)]
                       (-> s
                           (register sid (delay 1000))
                           (connect id  [:out sid] sid :in)
                           (connect sid :out       id  [:in sid])
                           (update :waiting conj sid))))
        rem-server (fn [s]
                     (let [sid (peek (:waiting s))]
                       (-> s
                           (unregister sid)
                           (disconnect id  [:out sid] sid :in)
                           (disconnect sid :out       id  [:in sid])
                           (update :waiting pop))))
        ingest     (fn [s x]
                     (reduce-kv (fn [s port v*]
                                  (case port
                                    :in (update s :job-queue into v*)
                                    (let [[_ sid] port]
                                      (-> s
                                          (update-in [:output :out] conj ['done (first v*)])
                                          (update :waiting conj sid)))))
                                s
                                x))
        maybe-add  (fn [s]
                     (if (> (- (count (:job-queue s)) threshold)
                            (count (:waiting s)))
                       (recur (add-server s))
                       s))
        maybe-rem  (fn [s]
                     (if (< (+ (count (:job-queue s)) threshold)
                            (count (:waiting s)))
                       (recur (rem-server s))
                       s))
        process    (fn [s]
                     (if (and (seq (:waiting   s))
                              (seq (:job-queue s)))
                       (let [sid (peek (:waiting   s))
                             job (peek (:job-queue s))]
                         (-> s
                             (update :waiting   pop)
                             (update :job-queue pop)
                             (update-in [:output [:out sid]] conj job)
                             recur))
                       s))]
    (executive-model
     (-> {:pending   []
          :waiting   []
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
    clojure.pprint/pprint)

;; Dynamic ports.

;; Port transducers.
