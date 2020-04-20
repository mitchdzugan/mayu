(ns mayu.frp.signal
  (:require [wayra.core :as w
             :refer [defnm defm mdo]]
            [mayu.frp.event :as e]))

(defn off! [s] ((:off! s)))
(defn inst! [s] ((:inst! s)))
(defn on! [s] ((:on! s)))
(defn hot-swap! [s e] ((:hot-swap! s) e))
(def changed :changed)
(defn consume! [s f] ((:consume! s) f))

(defnm step [k m]
  step-fn <- (w/asks :step-fn)
  ((or step-fn (fn [_ m] m)) k m))

(defm active-signal
  getter <- (w/asks :active-signal)
  (or getter (w/pure nil)))

(defnm default-setter [signal]
  (w/tell (fn [] (off! signal))))

(defnm set-active [signal]
  setter <- (w/asks :set-active)
  ((or setter default-setter) signal))

(defnm raw-from [init e-arg]
  signal <-
  (mdo
   active <- active-signal
   active --> [(hot-swap! active e-arg)
               active]
   let [changed (e/on! (e/Event))
        sval (atom init)
        e (atom e-arg)
        eoff (atom nil)
        on! (fn []
              (reset! eoff
                      (e/consume! @e #(when (not= @sval %1)
                                        (reset! sval %1)
                                        (e/push! changed %1)))))]
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
  (set-active signal)
  [signal])

(defnm from [init e]
  (step ::from (raw-from init e)))

(defn build [b]
  (let [{:keys [result writer]} (w/exec {} b)]
    {:off (fn [] (doseq [off writer]
                   (off)))
     :signal result}))

(defn unwrap-event [se]
  (e/join (inst! se) (e/flatten (changed se))))

(defn map [f s]
  (step ::map (raw-from (f (inst! s)) (e/map f (changed s)))))

(defnm reduce [r i-arg e]
  (step ::reduce
        (mdo active <- active-signal
             reader <- w/ask
             let [i (if active (inst! active) i-arg)]
             (raw-from i (e/reduce r i e)))))
