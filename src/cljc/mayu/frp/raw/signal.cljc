(ns mayu.frp.raw.signal
  (:require [mayu.frp.signal :as s]))

(def off! s/off!)
(def inst! s/inst!)
(def hot-swap! s/hot-swap!)
(def changed s/changed)
(def consume! s/consume!)
(def unwrap-event s/unwrap-event)
(def from (comp :signal s/build s/from))
(def const (comp :signal s/build s/const))
(def map (comp :signal s/build s/map))
(def reduce (comp :signal s/build s/reduce))
(def zip-with (comp :signal s/build s/zip-with))

