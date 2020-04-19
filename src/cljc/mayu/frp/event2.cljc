(ns mayu.frp.event2
  (:require [allpa.core :as a
             :refer [curry defprotomethod]]
            [mayu.async
             :refer [go go-loop timeout <! chan >! close!]]
            [wayra.monoid
             :refer [Monoid mappend maplus mempty]]
            #?(:clj [clojure.core :as core]
               :cljs [cljs.core :as core])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])))

(defrecord Val [val deps])
(defrecord Deps [deps])
(defrecord Off [])


(defrecord RawEvent [state])

(def next-event-id (atom 1))

(def never (->RawEvent (atom {:id 0 :on? true})))

(defn never? [e] (= 0 (:id @(:state e))))

(defn Event
  ([] (Event (fn [_] (fn [])) {}))
  ([on-required] (Event on-required true))
  ([on-required source?]
   (let [id @next-event-id]
     (swap! next-event-id inc)
     (->RawEvent (atom {:id id
                        :subs {}
                        :next-sub-id 1
                        :on-required on-required
                        :off-callback (fn [])
                        :source? source?
                        :pushes 1
                        :on? false
                        :num-awaiting 0
                        :awaiting-on (chan)})))))

(defprotomethod val? [_] Val true [Deps Off] false)

(defn send! [e msg]
  (when (not (never? e))
    (let [{:keys [state]} e
          {:keys [subs on? num-awaiting awaiting-on]} @state]
      (if on?
        (doseq [on (vals subs)]
          (on msg))
        (do (swap! state #(update %1 :num-awaiting inc))
            (go (>! awaiting-on {:msg msg :order num-awaiting})))))))


(defn on! [e]
  (let [{:keys [state]} e
        {:keys [on? awaiting-on num-awaiting]} @state]
    (when (and (not (never? e)) (not on?))
      (swap! state (curry assoc :on? true))
      (go-loop [left num-awaiting
                msgs {}]
        (if (> left 0)
          (let [{:keys [msg order]} (<! awaiting-on)]
            (recur (dec left) (assoc msgs order msg)))
          (do
            (close! awaiting-on)
            (doseq [id (range (count msgs))]
              (send! e (get msgs id)))))))
    e))

(defn off! [e]
  (when (not (never? e))
    (let [{:keys [state]} e
          {:keys [subs on? num-awaiting awaiting-on]} @state]
      (doseq [on (vals subs)]
        (on (->Off)))
      (swap! state (curry merge {:id 0 :subs nil :awaiting-on nil})))))

(defn push! [e val]
  (let [{:keys [state]} e
        {:keys [id pushes source?]} @state]
    (when (not source?)
      (throw (a/Err "Can only push to source event types")))
    (swap! state (curry update :pushes inc))
    (send! e (->Val val {id (inc pushes)}))))

(defn subscribe! [e on]
  (if (never? e)
    (fn [])
    (let [{:keys [state]} e
          {:keys [next-sub-id on-required]} @state
          sub-count (fn [] (count (:subs @state)))]
      (swap! state #(-> %1
                       (assoc :next-sub-id (inc next-sub-id))
                       (assoc-in [:subs next-sub-id] on)))
      (when (= 1 (sub-count))
        (swap! state (curry assoc :off-callback (on-required #(send! e %1)))))
      (fn []
        (swap! state (curry update :subs #(dissoc %1 next-sub-id)))
        (when (= 0 (sub-count))
          ((:off-callback @state)))))))

(defn consume! [e on] (subscribe! e (fn [msg]
                                      (when (val? msg) (on (:val msg))))))


(defn shadow [e]
  (if (never? e)
    never
    (on! (Event
          #(subscribe! e (fn [msg]
                           (when (val? msg)
                             (% (assoc msg :deps {})))))))))

(defn map [f e]
  (if (never? e)
    never
    (on! (Event
          #(subscribe! e (fn [msg]
                           (% (if (val? msg)
                                (update msg :val f)
                                msg))))
          false))))

(defn clone [e] (map identity e))

(defn reduce [r i e]
  (if (never? e)
    never
    (let [a-acc (atom i)]
      (on! (Event
            #(subscribe! e (fn [{:keys [val] :as msg}]
                             (if (val? msg)
                               (do (swap! a-acc (fn [acc] (r acc val)))
                                   (% (assoc msg :val @a-acc)))
                               (% msg))))
            false)))))

(defn filter [p e]
  (if (never? e)
    never
    (on! (Event
          #(subscribe! e
                       (fn [{:keys [val deps] :as msg}]
                         (cond (and (val? msg)
                                    (not (p val))) (% (->Deps deps))
                               :else (% msg))))
          false))))

(defn join [& events] (first events))

(defn join-skip-siblings [& events] (first events))

(defn flat-map [f e] e)

(extend-protocol Monoid
  RawEvent
  (mappend [e1 e2] (join e1 e2))
  (maplus [e1 e2] (join e1 e2))
  (mempty [_] never))
