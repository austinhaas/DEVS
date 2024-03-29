(ns pettomato.devs.lib.date
  #?(:cljs
     (:require
      [goog.i18n.DateTimeFormat.Format])))

(defn now
  "Returns a date object for the current time."
  []
  #?(:clj  (java.util.Date.)
     :cljs (js/Date.)))

#?(:cljs
   (def high-performance-timer-available?
     "True if the cljs run-time environment supports the high-performance timer,
  otherwise false."
     (try
      (boolean
       (and js/window
            (.-performance js/window)
            (.-timing (.-performance js/window))
            (.-navigationStart (.-timing (.-performance js/window)))))
      (catch :default e false))))

(defn timestamp
  "Returns an integer representing the number of milliseconds since
  _some_ epoch. This is intended to be used when you only care about
  the delta between timestamps, not what time they actually
  represent. In the case of CLJS, it will try to use a high precision
  timestamp, if available."
     []
     #?(:clj  (.getTime (java.util.Date.))
        :cljs (if high-performance-timer-available?
                (+ (.now (.-performance js/window))
                   (.-navigationStart (.-timing (.-performance js/window))))
                (.now js/Date))))

(defn format-date
  "Formats the supplied date with the date-format string. If date-format is not
  supplied, it defaults to HH:mm:ss.SSS."
  ([date] (format-date "HH:mm:ss.SSS" date))
  ([date-format date]
   #?(:clj  (-> (java.text.SimpleDateFormat. date-format) (.format date))
      :cljs (-> (goog.i18n.DateTimeFormat.   date-format) (.format date)))))
