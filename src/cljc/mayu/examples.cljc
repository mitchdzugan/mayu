(ns mayu.examples
  (:require [mayu.macros
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
  <[h2 "<[my-component 0]"]
  <[my-component 0]
  <[h2 "<[my-component 1]"]
  <[my-component 1]
  <[button "Test"] d-button >
  let [e (e/reduce inc 0 (dom/on-click d-button))
       {s :signal} (s/build (s/from 0 e))]
  <[dom/collect ::test $[e]=
    <[dom/bind s $[v]= (dom/text v)]
    <[div "in collector"]
    ]
  )
