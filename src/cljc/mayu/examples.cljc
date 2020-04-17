(ns mayu.examples
  (:require [allpa.core
             :refer [varg#]]
            [mayu.macros
             :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]))


(defui my-component [n]
  (= 0 n) --> <[span "loading"]
  <[ul $=
    <[dom/stash $=
      <[li "was"]
      <[li "out"]
      <[dom/keyed "Test" $=
        <[li "of"]]
      <[li "order"]
      ] stashed >
    <[li {:class "aclass"} "You!"]
    <[li "thought"]
    <[li "this"]
    <[dom/unstash stashed]]
  <[h3 "this is a header"]
  <[p "this is a test paragraph!!"]
  )

(defui my-ui []
  <[dom/collect-and-reduce ::score #(+ %1 %2) 0 $=
    <[button "Click for (+1)"] d-plus >
    s-score <- (dom/envs ::score)
    <[dom/bind s-score $[score]=
      <[div (str "Score: " score)]]
    <[button "Click for (-1)"] d-minus >
    (dom/emit ::score (e/map (varg# 1) (dom/on-click d-plus)))
    (dom/emit ::score (e/map (varg# -1) (dom/on-click d-minus)))])
