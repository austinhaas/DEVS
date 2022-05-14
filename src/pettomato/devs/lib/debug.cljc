(ns pettomato.devs.lib.debug
  #?(:cljs (:require-macros pettomato.devs.lib.debug)))

(defmacro ex-assert
  "Evaluates expr and throws an instance of ExceptionInfo if it does not
  evaluate to logical true."
  ([x]
     (when *assert*
       `(when-not ~x
          (throw (ex-info (str "Assert failed: " (pr-str '~x))
                          {})))))
  ([x message]
     (when *assert*
       `(when-not ~x
          (throw (ex-info (str "Assert failed: " ~message "\n" (pr-str '~x))
                          {})))))
  ([x message map]
     (when *assert*
       `(when-not ~x
          (throw (ex-info (str "Assert failed: " ~message "\n" (pr-str '~x))
                          ~map))))))
