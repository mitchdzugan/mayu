(ns mayu.frp
  (:require [allpa.core :as a
             :refer [deftagged defn-match fn-match match]]
            [mayu.async
             :refer [go timeout <!]]
            [wayra.monoid
             :refer [Monoid mappend maplus]]))

; Actions
(deftagged Push [info])
(deftagged Sub [on])
(deftagged Unsub [sub-id])
(deftagged GetAncestry [])

; Info
(deftagged NewVal [val ancestry])
(deftagged NewAncestry [ancestry])

(defrecord RawEvent [send! never?])

(defn-match event-on-action
  [(Sub on) (__ next-sub-id subs on-required :as state) send!]
  [(merge state
          {:next-sub-id (inc next-sub-id)
           :subs (merge subs {next-sub-id on})}
          (if (empty? subs) {:off-callback (on-required (comp send! Push))} {}))
   next-sub-id]

  [(Unsub sub-id) (__ subs off-callback :as state) _]
  [(do (when (= 1 (count subs))
         (off-callback))
       (assoc state :subs (dissoc sub-id subs)))]

  [(Push :info (__ :ancestry merge-ancestry :as info)) (__ subs ancestry event-id :as state) _]
  [(let [updated (merge (update ancestry event-id inc) merge-ancestry)]
     (doseq [[_ on] subs]
       (on (assoc info :ancestry updated)))
     (assoc state :ancestry updated))]

  [(GetAncestry) (__ ancestry :as state) _]
  [state ancestry])

(def a-next-event-id (atom 1))
(defn Event
  ([] (Event (fn [& args] (fn [& args])) {}))
  ([on-required] (Event on-required {}))
  ([on-required ancestry]
   (let [event-id @a-next-event-id
         a-state (atom {:ancestry (merge ancestry {event-id 0})
                        :event-id event-id
                        :next-sub-id 1
                        :subs {}
                        :on-required on-required})]
     (swap! a-next-event-id inc)
     (letfn [(send! [m]
               (let [[state res] (event-on-action m @a-state send!)]
                 (reset! a-state state)
                 res)
               )]
       (->RawEvent send! false)))))

(defn ancestry! [{:keys [send!]}] (send! (GetAncestry)))

(defn push! ([{:keys [send!]} v] (send! (Push (NewVal v {})))))

(defn consume-raw! [{:keys [send!]} on]
  (let [sub-id (send! (Sub on))]
    (fn [] (send! (Unsub sub-id)))))

(defn consume! [e on]
  (consume-raw! e (fn-match [(NewVal val)] (on val)
                            [_] nil)))

(defn fmap [f e]
  (let [init-ancestry (ancestry! e)]
    (Event #(consume-raw! e (fn-match
                             [(NewVal val ancestry)]
                             (% (NewVal (f val) ancestry))

                             [(NewAncestry ancestry)]
                             (% (NewAncestry ancestry))))
     init-ancestry)))

(defn filter [p e]
  (let [init-ancestry (ancestry! e)]
    (Event #(consume-raw! e (fn-match
                             [(NewVal val ancestry)]
                             (% ((if (p val)
                                   (partial NewVal val)
                                   NewAncestry)
                                 ancestry))

                             [(NewAncestry ancestry)]
                             (% (NewAncestry ancestry))))
           init-ancestry)))

(defn reduce [r i e]
  (let [a-agg (atom i)
        init-ancestry (ancestry! e)]
    (Event #(consume-raw! e (fn-match
                             [(NewVal val ancestry)]
                             (let [next (r @a-agg val)]
                               (reset! a-agg next)
                               (% (NewVal next ancestry)))

                             [(NewAncestry ancestry)]
                             (% (NewAncestry ancestry))
                             ))
           init-ancestry)))

(defn defer [ms e]
  (Event #(consume-raw! e (fn-match
                           [(NewVal val ancestry)]
                           (go (<! (timeout ms))
                               (% (NewVal val ancestry)))

                           [_] nil))))

(defn once [val]
  (Event #(go (<! (timeout 0))
              (% (NewVal val {})))))

(def never (->RawEvent (fn [_]) true))

(defn join [& arg-events]
  (match (vec (remove :never? arg-events))
         [] never
         [only] only
         events
         (let [a-ancestries (->> events
                                 (map ancestry!)
                                 vec
                                 atom)])
         (println "events")))

(extend-protocol Monoid
  RawEvent
  (mappend [e1 e2] (join e1 e2))
  (maplus [e1 e2] (join e1 e2)))

