(ns pettomato.devs.lib.date
  #?(:cljs
     (:require
      [goog.i18n.DateTimeFormat.Format])))

(defn now
  "Returns a date object for the current time."
  []
  #?(:clj  (java.util.Date.)
     :cljs (js/Date.)))

(defn ms
  "Converts a date object into milliseconds."
  [date]
  (.getTime date))

(defn timestamp
  "Returns an integer representing the number of milliseconds since
  _some_ epoch. This is intended to be used when you only care about
  the delta between timestamps, not what time they actually
  represent. And in the case of CLJS, it will try to use a high
  precision timestamp, if available."
     []
     #?(:clj  (.getTime (java.util.Date.))
        :cljs (or (.now (.-performance js/window))
                  (.getTime (js/Date.)))))

(defn format-date
  ([date] (format-date "HH:mm:ss.SSS" date))
  ([date-format date]
   #?(:clj  (-> (java.text.SimpleDateFormat. date-format) (.format date))
      :cljs (-> (goog.i18n.DateTimeFormat.   date-format) (.format date)))))
