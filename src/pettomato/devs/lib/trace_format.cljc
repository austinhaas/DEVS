(ns pettomato.devs.lib.trace-format
  "A library for formatting trace events."
  (:require [pettomato.devs.lib.string :refer [pad-left format-str]]))

(def ^:dynamic *time-width* 12)

(defn format-time [t]
  (str "[" (pad-left *time-width* \  (str t)) "]"))

(defn format-path [path]
  (let [c (count path)]
    (if (pos? c)
      (let [i (* c 1)
            w (apply str (repeat (- i 1) \ ))
            a "|-"]
        (str w a (last path)))
      "")))

(defn format-mail [mail]
  (zipmap (keys mail)
          (map vec (vals mail))))

(defn format-context-prefix [context]
  (let [{:keys [time path]} context]
    (format-str "%s %s"
                (format-time time)
                (format-path path))))


(defmulti format-event (juxt :method :type :event))


(defmethod format-event :default [event]
  (format-str "%s %s %s"
              (format-context-prefix event)
              (:method event)
              (or (:type event)
                  (:event event))))


(defmethod format-event [:initialize :enter nil] [event]
  (format-str "%s %s %s"
              (format-context-prefix event)
              (:method event)
              (:type event)))

(defmethod format-event [:initialize :exit nil] [event]
  (format-str "%s %s %s"
              (format-context-prefix event)
              (:method event)
              (:type event)))

(defmethod format-event [:collect-mail :enter nil] [event]
  (format-str "%s %s %s"
              (format-context-prefix event)
              (:method event)
              (:type event)))

(defmethod format-event [:collect-mail :exit nil] [event]
  (format-str "%s %s %s %s"
              (format-context-prefix event)
              (:method event)
              (:type event)
              (format-mail (:mail event))))

(defmethod format-event [:transition :enter nil] [event]
  (format-str "%s %s %s %s"
              (format-context-prefix event)
              (:method event)
              (:type event)
              (format-mail (:mail event))))

(defmethod format-event [:transition :exit nil] [event]
  (format-str "%s %s %s"
              (format-context-prefix event)
              (:method event)
              (:type event)))


(defmethod format-event [:initialize :event :add-model] [event]
  (let [{:keys [id model elapsed]} event]
    (format-str "%s %s %s %s"
                (format-context-prefix event)
                :add-model
                id
                [model elapsed])))

(defmethod format-event [:initialize :event :rem-model] [event]
  (let [{:keys [id]} event]
    (format-str "%s %s %s"
                (format-context-prefix event)
                :rem-model
                id)))

(defmethod format-event [:initialize :event :connect] [event]
  (let [{:keys [route]} event]
    (format-str "%s %s %s"
                (format-context-prefix event)
                :connect
                route)))

(defmethod format-event [:initialize :event :disconnect] [event]
  (let [{:keys [route]} event]
    (format-str "%s %s %s"
                (format-context-prefix event)
                :disconnect
                route)))

(defmethod format-event [:transition :event :add-model] [event]
  (let [{:keys [id model elapsed]} event]
    (format-str "%s %s %s %s"
                (format-context-prefix event)
                :add-model
                id
                [model elapsed])))

(defmethod format-event [:transition :event :rem-model] [event]
  (let [{:keys [id]} event]
    (format-str "%s %s %s"
                (format-context-prefix event)
                :rem-model
                id)))

(defmethod format-event [:transition :event :connect] [event]
  (let [{:keys [route]} event]
    (format-str "%s %s %s"
                (format-context-prefix event)
                :connect
                route)))

(defmethod format-event [:transition :event :disconnect] [event]
  (let [{:keys [route]} event]
    (format-str "%s %s %s"
                (format-context-prefix event)
                :disconnect
                route)))
