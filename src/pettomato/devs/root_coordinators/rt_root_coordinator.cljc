(ns pettomato.devs.root-coordinators.rt-root-coordinator
  (:require
   [pettomato.devs.lib.clock :as clock]
   [pettomato.devs.lib.date :refer [timestamp]]
   [pettomato.devs.lib.event-log :refer [pp-event-log]]
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.logging :refer [log-fn]]
   [pettomato.devs.lib.number :refer [infinity]]
   [pettomato.devs.root-coordinators.step-root-coordinator :refer [step-through step-root-coordinator]]
   [pettomato.devs.simulator :refer [initialize collect-mail transition time-of-last-event time-of-next-event]]
   [pettomato.devs.vars :refer [*sim-time*]]))

(defn- step-through-and-transition
  "Like step-through, but always invokes a transition at end time, even if the
  simulator is not imminent. This artificial transition serves to inform the
  simulator of the current sim-time."
  [sim end]
  (log/tracef "step-through-and-transition end: %s" end)
  (let [[sim event-log] (step-through sim end)]
    ;; If there happened to be a scheduled event at the current time, then there
    ;; is no need for the artificial transition. But if there wasn't, do the
    ;; artificial transition, and then step-though to the current time again,
    ;; just in case the artificial transition resulted in the sim becoming
    ;; imminent.
    (log/tracef "time-of-last-event: %s" (time-of-last-event sim))
    (if (= (time-of-last-event sim) end)
      [sim event-log]
      (let [sim              (binding [*sim-time* end] (transition sim {} end))
            [sim event-log'] (step-through sim end)]
        [sim (concat event-log event-log')]))))

(defn- step-through-to-current-time
  [rc]
  (let [{:keys [clock sim output-fn]} rc
        end                           (clock/get-time clock (timestamp))
        _                             (log/tracef "Updating sim-time to %s" end)
        [sim out]                     (step-through-and-transition sim end)]
    (output-fn out)
    (assoc rc
           :clock clock
           :sim   sim)))

(defn- spawn-update-thread! [rc]
  (.start
   (Thread.
    (let [out       *out*
          err       *err*
          log-fn    log/*log-function*
          log-level log/*log-level*]
      (fn []
        (binding [log/*log-function* log-fn
                  log/*log-level*    log-level
                  *out*              out
                  *err*              err]
          (log/trace "entering update thread")
          (loop [tl (timestamp)]
            (let [tn (+ tl (:update-ms @rc))]
              (Thread/sleep (- tn (timestamp)))
              (swap! rc step-through-to-current-time)
              (if (= :running (:status @rc))
                (recur tn)
                (log/trace "exiting update thread"))))))))))

;;; API

(defn rt-root-coordinator
  "Run a simulation in real-time.

  Options:

  start - Simulation start time (inclusive). Default: 0."
  [sim & {:keys [start scale output-fn update-ms]
          :or   {start     0
                 scale     1.0
                 output-fn (fn [event-log]
                             (when (seq event-log) (log/info "output"))
                             (pp-event-log event-log))
                 update-ms 1000}}]
  (atom
   {:clock     (clock/clock (timestamp) start scale)
    :sim       (step-root-coordinator sim :start start)
    :output-fn output-fn
    :update-ms update-ms
    :status    :stopped}))

(defn start! [rc]
  (swap! rc (fn [rc%]
              (log/infof "START rt-root-coordinator")
              (assert (= :stopped (:status rc%)) "Already started.")
              (spawn-update-thread! rc)
              (-> rc%
                  (update :clock clock/unpause (timestamp))
                  (assoc :status :running))))
  nil)

(defn stop! [rc]
  (swap! rc (fn [rc%]
              (log/infof "STOP rt-root-coordinator")
              (assert (= :running (:status rc%)) "Already stopped.")
              (-> rc%
                  ;; TODO: This freezes the clock immediately, but the update
                  ;; still plays out. This might look weird.
                  (update :clock clock/pause (timestamp))
                  (assoc :status :stopped))))
  nil)


(comment

  ;;(require '[pettomato.devs.examples.models :refer [generator]])
  ;;(require '[pettomato.devs.simulators.atomic-simulator :refer [atomic-simulator]])
  (require '[pettomato.devs.models.atomic-model :refer [atomic-model]])
  (require '[pettomato.devs.simulators.rt-atomic-simulator :refer [rt-atomic-simulator]])

  (defn rt-generator
    "A model that periodically emits value on a port labeled :out."
    [period value]
    (atomic-model
     (let [s {:sigma period}
           e 0]
       [s e])
     (fn int-update [s]
       (assoc s :sigma period))
     (fn ext-update [s e x]
       (update s :sigma - e))
     nil
     (constantly {:out [value]})
     :sigma))

  (binding [log/*log-function* log-fn
            log/*log-level*    :trace]
    (def rc (-> (rt-generator 1000 'tick)
                rt-atomic-simulator
                (rt-root-coordinator :update-ms 100
                                     :scale 1.0))))

  (binding [log/*log-function* log-fn
            log/*log-level*    :info]
    (start! rc))

  (binding [log/*log-function* log-fn
            log/*log-level*    :trace]
    (stop! rc))

  (.getState (:update-thread @rc))
)
