(ns pettomato.devs.lib.number)

(def infinity #?(:clj  Double/POSITIVE_INFINITY
                 :cljs (.-POSITIVE_INFINITY js/Number)))
