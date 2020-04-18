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
  <[p "this is a test paragraph!!"])

(defui change-score-button [change]
  <[button (str "Click for " change " points")] btn >
  (dom/emit ::score (e/map (varg# change) (dom/on-click btn))))

(defui score-display [score]
  <[div (str "Score: " score)])

(defui my-ui []
  <[dom/collect-and-reduce ::score #(+ %1 %2) 0 $=
    [(println "Rendering Component Top Level")]
    s-score <- (dom/envs ::score)
    <[dom/bind s-score $[score]=
      ; [(println "Bind 1")]
      <[dom/collect-and-reduce ::score2 #(+ %1 %2) 0 $=
        <[button "Inc scores"] btn >
        (dom/emit ::score2 (e/map (varg# 1) (dom/on-click btn)))
        s-score2 <- (dom/envs ::score2)
        <[dom/bind s-score2 $[score2]=
          ; [(println "Bind 2")]
          <[div (str "Score2: " score2)]]]
      <[change-score-button 1]
      <[score-display score]
      <[change-score-button -1]]])

