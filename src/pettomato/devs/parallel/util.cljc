(ns pettomato.devs.parallel.util)

(defn flatten-mail
  "k->port->vs -> [[k port v] ...]"
  [m]
  (for [[k port->vs] m
        [port vs]    port->vs
        v            vs]
    [k port v]))

(defn mail-equal? [m1 m2]
  (= (frequencies (flatten-mail m1))
     (frequencies (flatten-mail m2))))
