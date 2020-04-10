(ns mayu.core
  (:require [wayra.core :as w
             :refer [defnm defm mdo]]
            [mayu.frp.event :as frp]
            [mayu.attach :as attach]
            [mayu.examples :as examples]
            [cljs.pprint :refer [pprint]]
            ))

(defn logger [& args]
  (fn [val]
    (println (vec (concat [val] args)))))

(defn run-frp []
  (let [e1 (frp/Event)
        _ (frp/on! e1)
        e2 (->> e1
                (frp/filter #(< %1 6))
                (frp/fmap #(-> [%1 :e2])))
        e3 (->> e1
                (frp/filter #(< %1 5))
                (frp/fmap #(-> [%1 :e3])))
        e4 (->> e1
                (frp/filter #(< %1 3))
                (frp/fmap #(-> [%1 :e4])))
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
  (let [e1 (frp/Event)
        _ (pprint [:e1 e1])
        _ (frp/on! e1)
        e2 (frp/e-reduce #(+ %1 %2) 0 e1)
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
  #_(let [e (frp/preempt #(frp/fmap inc %1))]
    (frp/consume! e (logger :e-preempt))
    (frp/push! e 1)
    (frp/push! e 2)))


(defn mount-root []
  (run-frp)
  (attach/attach "app" {} examples/my-ui))

(defn init! []
  (mount-root))
