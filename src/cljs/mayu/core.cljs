(ns mayu.core
  (:require [wayra.core :as w
             :refer [defnm defm mdo]]
            [mayu.frp.event :as frp]
            [mayu.frp.raw.signal :as s]
            [mayu.attach :as attach]
            [mayu.examples :as examples]
            [cljs.pprint :refer [pprint]]
            ))

(defn logger [& args]
  (fn [val]
    (println (vec (concat [val] args)))))

(defn run-frp []
  #_(let [e (frp/reduce inc 0 (frp/timer 1000))
        s (s/from 0 e)]
    (s/consume! s (logger :s)))
  #_(let [e (frp/defer-off (frp/Event (fn [_]
                                      (println "ON")
                                      (fn [] (println "OFF")))))
        _ (frp/push! e 1)
        off1 (frp/consume! e (logger :e1))
        _ (frp/push! e 2)
        _ (off1)
        _ (frp/push! e 3)
        off2 (frp/consume! e (logger :e2))
        _ (frp/push! e 4)
        _ (off2)
        _ (frp/push! e 5)
        ])
  #_(let [e (frp/timer 1000)]
      (frp/consume! e (logger :time)))
  #_(let [e1 (frp/on! (frp/Event))
        e2 (frp/on! (frp/Event))
        e3 (frp/on! (frp/Event))
        e4 (frp/flat-map #(if (odd? %1) e3 e2) e1)]
    (frp/consume! e4 (logger :e4))
    (frp/push! e1 1)
    (frp/push! e3 [:e3 1])
    (frp/push! e3 [:e3 2])
    (frp/push! e3 [:e3 3])
    (frp/push! e2 [:e2 1])
    (frp/push! e2 [:e2 2])
    (frp/push! e2 [:e2 3])
    (frp/push! e1 2)
    (frp/push! e3 [:e3 4])
    (frp/push! e3 [:e3 5])
    (frp/push! e2 [:e2 4])
    (frp/push! e2 [:e2 5])
    )
  #_(let [e1 (frp/Event)
        _ (frp/on! e1)
        e2 (->> e1
                (frp/filter #(< %1 6))
                (frp/map #(-> [%1 :e2])))
        e3 (->> e1
                (frp/filter #(< %1 5))
                (frp/map #(-> [%1 :e3])))
        e4 (->> e1
                (frp/filter #(< %1 3))
                (frp/map #(-> [%1 :e4])))
        e5 (frp/join-skip-siblings e2 e3 e4)
        off (frp/consume! e5 (logger :e5))
        _ (frp/push! e1 1)
        _ (frp/push! e1 2)
        _ (frp/push! e1 3)
        _ (frp/push! e1 4)
        _ (frp/push! e1 5)
        _ (frp/push! e1 6)
        _ (frp/push! e1 0)
        _ (off)
        ])
  #_(let [e1 (frp/Event)
        _ (frp/on! e1)
        e2 (->> e1
                (frp/filter #(> %1 2))
                (frp/map #(-> [%1 :e2])))
        e3 (->> e1
                (frp/filter #(> %1 2))
                (frp/map #(-> [%1 :e3])))
        e4 (frp/join e2 e3)
        ; off (frp/subscribe! e4 #(if (frp/val? %1) nil (println (:ancestors %1))))
        off (frp/consume! e4 (logger :e4))
        _ (frp/push! e1 1)
        _ (frp/push! e1 2)
        _ (frp/push! e1 2)
        _ (frp/push! e1 2)
        _ (frp/push! e1 2)
        _ (frp/push! e1 2)
        _ (frp/push! e1 2)
        _ (frp/push! e1 3)
        _ (off)
        ])
  #_(let [e1 (frp/Event)
        _ (pprint [:e1 e1])
        _ (frp/on! e1)
        e2 (frp/reduce #(+ %1 %2) 20 e1)
        off (frp/consume! e2 (logger :e2))
        _ (frp/push! e1 1)
        _ (frp/push! e1 2)
        _ (frp/push! e1 3)
        _ (frp/push! e1 4)
        _ (frp/push! e1 5)
        _ (frp/push! e1 6)
        _ (frp/push! e1 0)
        _ (off)
        ])
  #_(let [e (frp/preempt #(frp/map inc %1))]
    (frp/consume! e (logger :e-preempt))
    (frp/push! e 1)
    (frp/push! e 2)))


(defn mount-root []
  (run-frp)
  (attach/attach "app" {} examples/my-ui))

(defn init! []
  (mount-root))
