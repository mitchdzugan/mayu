(ns mayu.mdom
  (:require [allpa.core
             :refer [deftagged]]))

(deftagged MText [s])
(deftagged MCreateElement [tag key path attrs children])
(deftagged MBind [a-bind])



