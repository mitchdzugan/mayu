(ns mayu.c-two
  (:require [mayu.macros
             :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]))

(defui c []
  <[dom/collect-reduce-and-bind :v inc 0 $[v]=
    <[button "Inner"] btn >
    <[p (str "Inner " v)]
    (dom/emit :v (dom/on-click btn))])

