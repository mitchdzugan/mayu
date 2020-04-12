(ns mayu.frp.signal
  (:require [wayra.core :as w
             :refer [defnm defm]]
            [mayu.frp.event :as e]))

(defnm from [init e-arg]
  let [changed (e/on! (e/Event))
       sval (atom init)
       e (atom e-arg)
       eoff (atom nil)
       on! (fn []
             (reset! eoff
                     (e/consume! @e #(when (not= @sval %1)
                                       (reset! sval %1)
                                       (e/push! changed %1)))))]
  (w/tell (fn [] (@eoff)))
  [(on!)
   {:off! (fn [] (@eoff))
    :inst! (fn [] @sval)
    :on! on!
    :hot-swap! (fn [e-next]
                 (@eoff)
                 (reset! e e-next)
                 (on!))
    :changed changed
    :consume! (fn [f]
                (f @sval)
                (e/consume! changed f))}])

(defn off! [s] ((:off! s)))
(defn inst! [s] ((:inst! s)))
(defn on! [s] ((:on! s)))
(defn hot-swap! [s e] ((:hot-swap! s) e))
(def changed :changed)
(defn consume! [s f] ((:consume! s) f))

(defn map [f s] (from (f (inst! s)) (e/map f (changed s))))

(defn build [b]
  (let [{:keys [result writer]} (w/exec {} b)]
    {:off (fn [] (doseq [off writer]
                   (off)))
     :signal result}))
