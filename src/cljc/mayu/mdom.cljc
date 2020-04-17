(ns mayu.mdom)

(defrecord MText [s])
(defrecord MCreateElement [tag key path attrs children])
(defrecord MBind [signal])



