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
  (dom/emit ::score (e/map (varg# change) (dom/on-click btn)))
  [(dom/on-click btn)])

(defui score-display [score]
  <[div (str "Score: " score)])

(defui scores []
  <[dom/collect-and-reduce ::score #(+ %1 %2) 0 $=
    [(println "Rendering Component Top Level")]
    s-score <- (dom/envs ::score)
    <[dom/bind s-score $[score]=
      <[dom/collect-and-reduce ::score2 #(+ %1 %2) 0 $=
        <[button "Inc scores"] btn >
        (dom/emit ::score2 (e/map (varg# 1) (dom/on-click btn)))
        s-score2 <- (dom/envs ::score2)
        <[dom/bind s-score2 $[score2]=
          <[div (str "Score2: " score2)]]]
      <[change-score-button 1]>
      <[score-display score]
      <[change-score-button -1]>]])

(defui error-alert []
  <[div {:class "uk-alert-danger"} $=
    <[p "An unexpected error occurred"]])

(defui progress-bar [value max]
  <[progress {:class "uk-progress" :value value :max max}])

(defui countdown []
  s-timer <- (s/reduce inc 0 (e/timer 300))
  <[dom/collect-and-reduce ::error #(-> %2) nil $=
    <[div {:class "uk-card uk-card-default uk-card-body"} $=
      <[h3 {:class "uk-card-title"} "Content"]
      <[div {:class "uk-card-body"} $=
        s-error <- (dom/envs ::error)
        <[dom/bind s-error $[error]=
          <[dom/bind s-timer $[timer]=
            (not (nil? error)) --> <[error-alert]
            (<= timer 10)      --> <[progress-bar timer 10]
            <[table {:class "uk-table"} $=
              <[thead $=
                <[tr $=
                  <[th "Column 1"]
                  <[th "Column 2"]
                  <[th "Column 3"]]]
              <[tbody $=
                <[tr $=
                  <[td "Data 1.a"]
                  <[td "Data 2.a"]
                  <[td "Data 3.a"]]
                <[tr $=
                  <[td "Data 1.b"]
                  <[td "Data 2.b"]
                  <[td "Data 3.b"]]]]]]]
      <[div {:class "uk-card-footer"} $=
        <[a {:href "#" :class "uk-button uk-button-text"} "Set Error"] e-btn >
        (dom/emit ::error (e/map (varg# true) (dom/on-click e-btn)))]]])

(defui alert-system [e-alert]
  let [e-alert-off (e/map (varg# nil) (e/defer e-alert 2000))]
  s-alert <- (s/from nil (e/join e-alert e-alert-off))
  <[dom/bind s-alert $[alert]=
    <[dom/when (not (nil? alert)) $=
      <[div {:class "fixed"} $=
        <[div {:class "uk-card uk-card-default uk-card-body"} $=
          <[dom/unstash alert]]]]])

(defui add-alert-btn [btn-label]
  context-text <- (dom/envs ::context-text)
  <[dom/stash $= <[div context-text]] alert-stash >
  <[button btn-label] alert-btn >
  (dom/emit ::alert (e/map (varg# alert-stash) (dom/on-click alert-btn))))

(defui stash-demo3 []
  <[dom/collect ::alert $[e-alert]=
    <[dom/assoc-env ::context-text "From First Button" $=
      <[add-alert-btn "button 1"]]
    <[p "Some other text"]
    <[dom/assoc-env ::context-text "From Another Place" $=
      <[div $=
        <[p "Random Text..."]
        <[add-alert-btn "another button"]]]
    <[alert-system e-alert]])

(defui stash-demo2 []
  <[dom/assoc-env ::message "Bad Text" $=
    <[dom/assoc-env ::message "Good Text" $=
      <[dom/stash $=
        message <- (dom/envs ::message)
        <[p message]
        ] stashed >
      <[dom/unstash stashed]
      [stashed]
      ] stashed >
    <[dom/unstash stashed]])

(defui stash-demo1 []
  <[ul $=
    <[dom/stash $=
      <[li "appear"]
      <[li "in"]
      <[li "order"]
      ] stashed >
    <[li "These"]
    <[li "messages"]
    <[li "will"]
    <[dom/unstash stashed]])

(defui offs-test []
  <[dom/collect-and-reduce ::score #(+ %1 %2) 0 $=
    [(println "Rendering Component Top Level")]
    s-score <- (dom/envs ::score)
    <[dom/bind s-score $[score]=
      <[dom/keyed score $=
        <[dom/collect-and-reduce ::score2 #(+ %1 %2) 0 $=
          <[button "Inc scores"] btn >
          (dom/emit ::score2 (e/map (varg# 1) (dom/on-click btn)))
          s-score2 <- (dom/envs ::score2)
          <[dom/bind s-score2 $[score2]=
            s-timer <- (s/reduce inc 0 (e/timer 300))
            <[dom/bind s-timer $[timer]=
              <[p (str timer)]]
            <[div (str "Score2: " score2)]]]]
      <[change-score-button 1]>
      <[score-display score]
      <[change-score-button -1]>]])

(defui my-ui []
  <[offs-test])

