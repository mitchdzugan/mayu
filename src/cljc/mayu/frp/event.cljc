(ns mayu.frp.event
  (:require [mayu.frp.impl.event :as e]
            [mayu.frp.signal :as s]))

(defn Push [val src count]
  (e/->Push val src count))

(def never e/never)

(def never? e/never?)

(defn Event
  ([] (e/Event))
  ([on-req]
   (e/Event (fn [send-self!]
              (on-req #(send-self! (e/->Push %1 ::e/self :next)))))))

(def on! e/on!)

(def off! e/off!)

(def push! e/push!)

(def consume! e/consume!)

(def shadow e/shadow)

(def map e/map)

(def reduce e/reduce)

(def filter e/filter)

(def join e/join)

(def join-skip-siblings e/join-skip-siblings)

(def flat-map e/flat-map)

(def flatten e/flatten)

(def preempt e/preempt)

(def defer-off e/defer-off)

(def throttle e/throttle)

(def defer e/defer)

(def timer e/timer)

(defn tag-with [f e s]
  (->> e
       (map #(-> [true %1]))
       (join-skip-siblings (->> (s/changed s)
                                (map #(-> [false %1]))))
       (filter #(nth %1 0))
       (map #(f (s/inst! s) (nth %1 1)))))

(def tag (partial tag-with (fn [v _] v)))
