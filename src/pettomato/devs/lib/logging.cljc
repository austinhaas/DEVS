(ns pettomato.devs.lib.logging
  (:require
   [pettomato.devs.lib.log :as log]
   [pettomato.devs.lib.string :refer [pad-left format-str]]
   [pettomato.devs.vars :refer [*path* *sim-time*]]))

(def ^:dynamic *log-time-width* 6)

(defn- format-time [t]
  (str "[" (pad-left *log-time-width* \  (str t)) "]"))

(defn- format-path [path]
  (let [c (count path)]
    (if (pos? c)
      (let [i (* c 2)
            w (apply str (repeat (- i 2) \ ))
            a "|-"]
        (str w a (last path)))
      "")))

(def log-fn
  (fn [m]
    (some-> m
      log/log-level-filter
      log/add-date
      log/format-date
      (assoc :time *sim-time*)
      (assoc :path *path*)
      (update :time format-time)
      (update :path format-path)
      ((fn [context]
         (let [{:keys [date time path message]} context
               space (if (pos? (count path)) " " "")]
           (format-str "%s %s %s%s%s"
                       date time path space message))))
      log/print-fn)))
