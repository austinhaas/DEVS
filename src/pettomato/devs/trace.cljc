(ns pettomato.devs.trace
  "Tracing for DEVS."
  #?(:cljs (:require-macros pettomato.devs.trace)))

(def ^:dynamic *trace-enabled* false)

(def ^:dynamic *context* {:path [:root]})

(def ^:dynamic *handler* (constantly nil))


(defn trace [event]
  (*handler* (merge *context* event))
  nil)


(defn initialize-enter [context [sim t]]
  (assoc context
         :method :initialize
         :type :enter
         :sim sim
         :time t))

(defn initialize-exit [context sim]
  (-> context
      (assoc :type :exit :sim sim)))

(defn collect-mail-enter [context [sim t]]
  (assoc context
         :method :collect-mail
         :type :enter
         :sim sim
         :time t))

(defn collect-mail-exit [context mail]
  (-> context
      (assoc :type :exit :mail mail)
      (dissoc :sim)))

(defn transition-enter [context [sim mail t]]
  (assoc context
         :method :transition
         :type :enter
         :sim sim
         :mail mail
         :time t))

(defn transition-exit [context sim]
  (-> context
      (assoc :type :exit :sim sim)
      (dissoc :mail)))


(defmacro with-trace [enter-fn exit-fn args & body]
  `(binding [*context* (~enter-fn *context* ~args)]
     (trace *context*)
     (let [result# (do ~@body)]
       (binding [*context* (~exit-fn *context* result#)]
         (trace *context*))
       result#)))

(defmacro with-initialize [[sim t] & body]
  `(with-trace
     initialize-enter
     initialize-exit
     [~sim ~t]
     ~@body))

(defmacro with-collect-mail [[sim t] & body]
  `(with-trace
     collect-mail-enter
     collect-mail-exit
     [~sim ~t]
     ~@body))

(defmacro with-transition [[sim mail t] & body]
  `(with-trace
     transition-enter
     transition-exit
     [~sim ~mail ~t]
     ~@body))


(defn add-model [id [model elapsed]]
  (trace {:type    :event
          :event   :add-model
          :id      id
          :model   model
          :elapsed elapsed}))

(defn rem-model [id]
  (trace {:type  :event
          :event :rem-model
          :id    id}))

(defn connect [route]
  (trace {:type  :event
          :event :connect
          :route route}))

(defn disconnect [route]
  (trace {:type  :event
          :event :disconnect
          :route route}))


(defn get-id [] (peek (get *context* :path)))

(defn get-parent [] (peek (pop (get *context* :path))))

(defn get-path [] (get *context* :path))

(defn get-sim-time [] (get *context* :time))

(defn get-devs-method [] (get *context* :method))
