(ns mayu.examples
  (:require [mayu.macros
             :refer [defui]]
            [mayu.dom :as dom
             :refer [stash apply]]))


(defui my-component [n]
  (= 0 n) --> <[span "loading"]
  <[ul $=
    <[stash $=
      <[li "was"]
      <[li "out"]
      <[li "of"]
      <[li "order"]
      ] stashed >
    <[li "You"]
    <[li "thought"]
    <[li "this"]
    <[apply stashed]]
  <[h3 "this is a header"]
  <[p "this is a test paragraph!!"]
  )

(defui my-ui []
  <[h2 "<[my-component 0]"]
  <[my-component 0]
  <[h2 "<[my-component 1]"]
  <[my-component 1]
  )
