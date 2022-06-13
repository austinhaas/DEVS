(ns pettomato.devs.lib.print
  #?(:cljs (:require-macros [pettomato.devs.lib.print]))
  (:require
   #?(:clj [clojure.pprint :as pprint]
      :cljs [cljs.pprint :as pprint]))
  #?(:clj (:import [java.io Writer])))

(defmacro add-writer-clj [type]
  `(defmethod print-method ~type [^{:tag ~type} x# ^java.io.Writer w#]
     (.write w# (.toString x#))))

(defmacro add-writer-cljs [type]
  `(extend-protocol ~'IPrintWithWriter
     ~type
     (~'-pr-writer [x# w# _#]
      (~'write-all w# (.toString x#)))))

(defmacro add-pprint-dispatch-clj [type]
  `(defmethod pprint/simple-dispatch ~type [^{:tag ~type} x#]
     (.write *out* (.toString x#))))

(defmacro add-pprint-dispatch-cljs [type]
  ;; This is a hack. There doesn't seem to be a simple way to extend
  ;; the pprint method for CLJS.
  `(let [old# cljs.pprint/*print-pprint-dispatch*
         new# (fn [x#]
                (if (instance? ~type x#)
                  (#'cljs.pprint/pprint-simple-default x#)
                  (old# x#)))]
     (cljs.pprint/set-pprint-dispatch new#)))

(defmacro add-print-handlers-clj [type]
  `(do
     (add-writer-clj ~type)
     (add-pprint-dispatch-clj ~type)))

(defmacro add-print-handlers-cljs [type]
  `(do
     (add-writer-cljs ~type)
     (add-pprint-dispatch-cljs ~type)))
