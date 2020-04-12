(ns mayu.examples
  (:require [mayu.macros
             :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.dom :as dom]))


(defui my-component [n]
  (= 0 n) --> <[span "loading"]
  <[ul $=
    <[dom/stash $=
      <[li "was"]
      <[li "out"]
      <[dom/keyed "TEST" $= <[li "of"]]
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
  let [e1 (e/fmap (fn [_] :e1) (e/timer 1200))
       e2 (e/fmap (fn [_] :e2) (e/timer 800))]
  <[h2 "<[my-component 0]"]
  <[my-component 0]
  <[h2 "<[my-component 1]"]
  <[my-component 1]
  <[dom/collect ::test $[e]=
    ; [(e/consume! e #(println [:collected %1]))]
    (dom/emit ::test e1)
    (dom/emit ::test e2)
    <[div "in collector"]
    ]
  )
