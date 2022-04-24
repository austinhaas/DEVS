(ns pettomato.devs.lib.log
  "Trivial logging for Clojure and Clojurescript.

  Standard logging functions are provided, but the user is encouraged to create
  custom functions to add additional structured data, and to implement an
  alternative *log-function* to handle those extensions."
  (:require
   [clojure.string :as str]
   [pettomato.devs.lib.date :as date]
   [pettomato.devs.lib.string :refer [format-str]]))

;;------------------------------------------------------------------------------
;; Building blocks

(def print-fn
  #?(:clj  println
     :cljs js/console.log))

(defn format-date [context] (update context :date date/format-date))

(defn add-date [context] (assoc context :date (date/now)))

(defn default-formatter [context]
  (let [{:keys [date level message]} context]
    (format-str "%s %s %s" date level message)))

;;------------------------------------------------------------------------------
;; Core functionality

(declare log-level-filter)

(def ^:dynamic *log-function*
  "The implementation of the log function. This can be dynamically bound to an
  arbitrary function that takes a single argument, but to be compatible with the
  standard log functions below, it should be a function that takes a map with at
  least the fields :level and :message. The root binding checks the log level
  and, if it is enabled, it prints the current time, the value of :level, and
  the value of :message. Alternative implementations could change the format;
  write to multiple other destinations, such as files and databases; call
  another logger implementation; or store in an in-memory database, for
  example."
  (fn [m]
    (some-> m
      log-level-filter
      add-date
      format-date
      default-formatter
      print-fn)))

(defn log
  "Calls *log-function* on context."
  [context]
  (*log-function* context))

;;------------------------------------------------------------------------------
;; Standard logging functions

(def ^:dynamic *log-level*
  "The log level. This can be bound to :trace, :debug:, :info, :warn, :error,
  or :fatal. The root binding is :info."
  :info)

(def log-level->int
  {:trace 0
   :debug 1
   :info  2
   :warn  3
   :error 4
   :fatal 5})

(defn log-level-enabled? [level] (<= (log-level->int *log-level*)
                                     (log-level->int level)))

(defn log-level-filter [context]
  (when (log-level-enabled? (:level context))
    context))

(defn trace [& args] (log {:level :trace :message (str/join " " args)}))
(defn debug [& args] (log {:level :debug :message (str/join " " args)}))
(defn info  [& args] (log {:level :info  :message (str/join " " args)}))
(defn warn  [& args] (log {:level :warn  :message (str/join " " args)}))
(defn error [& args] (log {:level :error :message (str/join " " args)}))
(defn fatal [& args] (log {:level :fatal :message (str/join " " args)}))

(defn tracef [fmt & args] (trace (apply format-str fmt args)))
(defn debugf [fmt & args] (debug (apply format-str fmt args)))
(defn infof  [fmt & args] (info  (apply format-str fmt args)))
(defn warnf  [fmt & args] (warn  (apply format-str fmt args)))
(defn errorf [fmt & args] (error (apply format-str fmt args)))
(defn fatalf [fmt & args] (fatal (apply format-str fmt args)))
