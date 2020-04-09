(ns mayu.core
  (:require [wayra.core :as w
             :refer [defnm defm mdo]]
            [mayu.frp :as frp]
            [mayu.attach :as attach]
            [mayu.examples :as examples]
            [cljs.pprint :refer [pprint]]))

(defn run-frp []
  (let [e0 (frp/Event)
        e1 (frp/fmap inc e0)
        e2 (frp/filter odd? e1)
        e3 (frp/reduce + 0 e2)
        e4 (frp/defer 2000 e3)
        ]
    (frp/consume! e0 #(println [:e0 %1]))
    (frp/consume! e1 #(println [:e1 %1]))
    (frp/consume! e2 #(println [:e2 %1]))
    (frp/consume! e3 #(println [:e3 %1]))
    (frp/consume! e4 #(println [:e4 %1]))
    (println (frp/join frp/never frp/never e1 e2))
    (frp/push! e0 0)
    (frp/push! e0 1)
    (frp/push! e0 2)
    (frp/push! e0 3)
    (frp/push! e0 4)
    (frp/push! e0 5)
    (frp/push! e0 6)
    (frp/push! e0 7)
    (frp/push! e0 8)
    (frp/push! e0 9)))


(defn mount-root []
  (attach/attach "app" {} examples/my-ui))

(defn init! []
  (mount-root))
