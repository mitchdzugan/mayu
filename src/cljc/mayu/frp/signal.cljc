(ns mayu.frp.signal
  (:require [allpa.core
             :refer [varg#]]
            [wayra.core :as w
             :refer [defnm defm mdo]]
            #?(:clj [clojure.core :as core]
               :cljs [cljs.core :as core])
            [mayu.frp.impl.event :as e]))

(defn off! [s] ((:off! s)))
(defn inst! [s] ((:inst! s)))
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

(defnm raw-from [get-init e-arg]
  signal <-
  (mdo
   active <- active-signal
   active --> [(hot-swap! active e-arg)
               active]
   let [e-internal (e/on! (e/Event))
        sval (atom (get-init))
        changed (->> e-internal
                     e/flatten
                     (e/filter #(not= %1 @sval))
                     (e/map #(do (reset! sval %1)
                                 %1)))
        off! (let [unsub (e/consume! changed (fn [x]))]
               (fn []
                 (unsub)
                 (e/off! changed)))]
   [(e/push! e-internal e-arg)
    {:off! off!
     :inst! (fn [] @sval)
     :hot-swap! (fn [e-next]
                  (e/push! e-internal e-next))
     :changed changed
     :consume! (fn [f]
                 (f @sval)
                 (e/consume! changed f))}])
  (set-active signal)
  [signal])

(defnm from [init e]
  (step ::from (raw-from #(-> init) e)))

(defnm const [val]
  (step ::const (raw-from #(-> val) e/never)))

(defn build [b]
  (let [{:keys [result writer]} (w/exec {} b)]
    {:off (fn [] (doseq [off writer]
                   (off)))
     :signal result}))

(defn unwrap-event [se]
  (e/join-skip-siblings (inst! se) (e/flatten (changed se))))

(defn map [f s]
  (step ::map (raw-from #(f (inst! s)) (e/map f (changed s)))))

(defnm reduce [r i-arg e]
  (step ::reduce
        (mdo active <- active-signal
             reader <- w/ask
             let [i (if active (inst! active) i-arg)]
             (raw-from #(-> i) (e/reduce r i e)))))

(defnm zip-with [f & sigs]
  (step ::zip-with
        (mdo let [e-changed (apply e/join-skip-siblings (core/map changed sigs))
                  val! (varg# (apply f (core/map inst! sigs)))]
             (raw-from val! (e/map val! e-changed)))))

(defnm shadow [s]
  (step ::shadow (raw-from #(inst! s) (e/shadow (changed s)))))
