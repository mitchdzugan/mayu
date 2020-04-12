(ns mayu.frp.event
  (:require [allpa.core :as a
             :refer [curry deftagged defn-match fn-match match]]
            [mayu.async
             :refer [go go-loop timeout <! chan >! close!]]
            [wayra.monoid
             :refer [Monoid mappend maplus mempty]]
            #?(:clj [clojure.core :as core]
               :cljs [cljs.core :as core])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            ))

(defrecord RawEvent [atom])

(def next-event-id (atom 1))

(def never (->RawEvent (atom {:id 0 :on? true})))

(defn never? [e] (= 0 (:id @(:atom e))))

(deftagged Val [val [ancestors {}]])
(deftagged Ancestors [ancestors])

(defn Event
  ([] (Event (fn [_] (fn [])) {}))
  ([on-required] (Event on-required {}))
  ([on-required ancestors]
   (let [id @next-event-id]
     (swap! next-event-id inc)
     (->RawEvent (atom {:id id
                        :subs {}
                        :next-sub-id 1
                        :on-required on-required
                        :off-callback (fn [])
                        :ancestors ancestors
                        :pushes 1
                        :on? false
                        :num-awaiting 0
                        :awaiting-on (chan)})))))

(defn-match val?
  [(Val)] true
  [_] false)

(defn send! [e msg]
  (if (never? e)
    nil
    (let [{:keys [atom]} e
          {:keys [type val] merge-ancestors :ancestors} msg
          {:keys [id pushes ancestors subs on? num-awaiting awaiting-on]} @atom
          new-pushes (if (val? msg) (inc pushes) pushes)
          new-ancestors (merge ancestors merge-ancestors)]
      (if on?
        (do (when (contains? ancestors id)
              (throw (a/Err "Resursive Event Encountered")))
            (when (or (val? msg)
                      (not= new-pushes pushes)
                      (not= new-ancestors ancestors))
              (swap! atom (curry merge {:pushes new-pushes
                                        :ancestors new-ancestors}))
              (doseq [on (vals subs)]
                (on (assoc-in msg [:ancestors id] new-pushes)))))
        (do (swap! atom #(update %1 :num-awaiting inc))
            (go (>! awaiting-on {:msg msg :order num-awaiting})))))))

(defn on! [e]
  (let [{:keys [atom]} e
        {:keys [on? awaiting-on num-awaiting]} @atom]
    (when (not on?)
      (swap! atom (curry assoc :on? true))
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

(defn push! [e val] (send! e (Val val)))

(defn subscribe! [e on]
  (if (never? e)
    (fn [])
    (let [{:keys [atom]} e
          {:keys [next-sub-id on-required]} @atom
          sub-count (fn [] (count (:subs @atom)))]
      (swap! atom #(-> %1
                       (assoc :next-sub-id (inc next-sub-id))
                       (assoc-in [:subs next-sub-id] on)))
      (when (= 1 (sub-count))
        (swap! atom (curry assoc :off-callback (on-required #(send! e %1)))))
      (fn []
        (swap! atom (curry update :subs #(dissoc %1 next-sub-id)))
        (when (= 0 (sub-count))
          ((:off-callback @atom)))))))

(defn consume! [e on] (subscribe! e (fn-match [(Val val)] (on val) [_] nil)))

(defn shadow [e-src]
  (if (never? e-src)
    never
    (on! (Event
          #(subscribe! e-src
                       (fn-match
                        [(Val :as msg)]
                        (% (assoc msg :ancestors {}))

                        [_] nil))))))

(defn fmap [f e-src]
  (if (never? e-src)
    never
    (let [{:keys [atom]} e-src
          {:keys [id ancestors pushes]} @atom]
      (on! (Event
            #(subscribe! e-src
                         (fn-match
                          [(Val :as msg)]
                          (% (update msg :val f))

                          [msg] (% msg)))
            (assoc ancestors id pushes))))))

(defn reduce [r i e-src]
  (if (never? e-src)
    never
    (let [a-acc (atom i)
          {:keys [atom]} e-src
          {:keys [id ancestors pushes]} @atom]
      (on! (Event
            #(subscribe! e-src
                         (fn-match
                          [(Val val :as msg)]
                          (do (swap! a-acc (fn [acc] (r acc val)))
                              (% (assoc msg :val @a-acc)))

                          [msg] (% msg)))
            (assoc ancestors id pushes))))))


(defn filter [p e-src]
  (if (never? e-src)
    never
    (let [{:keys [atom]} e-src
          {:keys [id ancestors pushes]} @atom
          ]
      (on! (Event
            #(subscribe! e-src
                         (fn-match
                          [(Val val :as msg)]
                          (if (p val)
                            (% msg)
                            (% (Ancestors (:ancestors msg))))

                          [msg] (% msg)))
            (assoc ancestors id pushes))))))

(defn to-ancestors [ancestor-data]
  (a/map-values #(apply min (-> %1 :counts vals))
                @ancestor-data)
  )

(defn process-message [ancestor-data send-self! e-src]
  (fn [msg]
    (let [{:keys [id]} (:atom e-src)
          {:keys [type val ancestors]} msg
          awaiting (atom 1)

          dec-awaiting
          (fn [sent-sibling?]
            (swap! awaiting dec)
            (if (= 0 @awaiting)
              (do
                (send-self! (-> msg
                                (assoc :ancestors (to-ancestors))
                                (assoc :val {:sent-sibling? sent-sibling?
                                             :val val})))
                (or sent-sibling? (val? msg)))
              sent-sibling?))

          prev (to-ancestors)]
      (doseq [[anc-id count] ancestors]
        (swap! ancestor-data #(assoc-in %1 [anc-id :counts id] count)))
      (let [curr (to-ancestors)]
        (doseq [[anc-id count] ancestors]
          (cond
            (and (< (get prev anc-id) count)
                 (< (get curr anc-id) count))
            (do (swap! awaiting inc)
                (when (val? msg)
                  (swap! ancestor-data (curry update-in [anc-id :stashed]
                                              #(conj %1 dec-awaiting)))))
            (< (get prev anc-id) count)
            (do (core/reduce #(%2 %1) (val? msg)
                             (get-in @ancestor-data [anc-id :stashed]))
                (swap! ancestor-data #(assoc-in %1 [anc-id :stashed]
                                                '()))))))
      (dec-awaiting false))))


(defn raw-join [& arg-events]
  (let [events (remove never? arg-events)

        event-on
        (fn [ancestor-data to-ancestors send-self! {:keys [id]} msg]
          (let [{:keys [type val ancestors]} msg
                awaiting (atom 1)

                dec-awaiting
                (fn [sent-sibling?]
                  (swap! awaiting dec)
                  (if (= 0 @awaiting)
                    (do
                      (send-self! (-> msg
                                      (assoc :ancestors (to-ancestors))
                                      (assoc :val {:sent-sibling? sent-sibling?
                                                   :val val})))
                      (or sent-sibling? (val? msg)))
                    sent-sibling?))

                prev (to-ancestors)]
            (doseq [[anc-id count] ancestors]
              (swap! ancestor-data #(assoc-in %1 [anc-id :counts id] count)))
            (let [curr (to-ancestors)]
              (doseq [[anc-id count] ancestors]
                (cond
                  (and (< (get prev anc-id) count)
                       (< (get curr anc-id) count))
                  (do (swap! awaiting inc)
                      (when (val? msg)
                        (swap! ancestor-data (curry update-in [anc-id :stashed]
                                                    #(conj %1 dec-awaiting)))))
                  (< (get prev anc-id) count)
                  (do (core/reduce #(%2 %1) (val? msg)
                                   (get-in @ancestor-data [anc-id :stashed]))
                      (swap! ancestor-data #(assoc-in %1 [anc-id :stashed]
                                                      '()))))))
            (dec-awaiting false))
          )

        on-required
        (fn [events ancestor-data to-ancestors send-self!]
          (let [offs
                (->> events
                     (core/map (fn [event]
                                 (subscribe! event
                                             #(event-on ancestor-data
                                                        to-ancestors
                                                        send-self!
                                                        @(:atom event)
                                                        %1))))
                     vec)]
            (fn [] (doseq [off offs] (off)))))

        perform-join
        (fn [events]
          (let [init-ancestor-data
                (core/reduce
                 (fn [data event]
                   (let [atom (:atom event)
                         {:keys [id ancestors pushes]} @atom]
                     (reduce-kv
                      (fn [data anc-id pushes]
                        (assoc-in data [anc-id :counts id] pushes))
                      data
                      (assoc ancestors id pushes))))
                 {}
                 events)
                ancestor-data (atom init-ancestor-data)
                to-ancestors (fn []
                               (a/map-values #(apply min (-> %1 :counts vals))
                                             @ancestor-data)
                               )]
            (on! (Event #(on-required events ancestor-data to-ancestors %1)
                        (to-ancestors)))))]

    (match events
           [] never
           [event] (fmap #(-> {:val %1 :sent-sibling? false}) event)
           events (perform-join events))))

(defn join [& events] (fmap :val (apply raw-join events)))

(defn join-skip-siblings [& events]
  (->> (apply raw-join events)
       (filter (comp not :sent-sibling?))
       (fmap :val)))

(defn preempt
  ([f] (preempt identity f))
  ([from-res f]
   (let [e-channel (chan)
         off-channel (chan)
         e-init (Event (fn [send-self!]
                         (go (let [e (<! e-channel)
                                   {:keys [ancestors]} @(:atom e)
                                   off (subscribe! e send-self!)
                                   ]
                               (on! e)
                               (>! off-channel off)))
                         (fn [] (go ((<! off-channel))))))
         res (f e-init)
         e (from-res res)
         atom-init (:atom e-init)
         {:keys [ancestors]} @(:atom e)]
     (swap! atom-init #(assoc %1 :ancestors ancestors))
     (on! e-init)
     (swap! (:atom e) #(merge %1 {:awaiting-on (chan)
                                  :on? false}))
     (go (>! e-channel e))
     res)))

(defn timer [ms]
  (let [c (chan 1)]
    (on! (Event #(do (go (>! c :on))
                     (go-loop []
                       (let [msg (<! c)]
                         (when (not= :off msg)
                           (%1 (Val :on))
                           (<! (timeout ms))
                           (>! c :on)
                           (recur))))
                     (fn [] (go (>! c :off))))))))

(defn flat-map [f e]
  (let [curr-off (atom (fn []))
        full-off (atom (fn []))
        {:keys [atom]} e
        {:keys [id ancestors pushes]} @atom]
    (on! (Event
          (assoc ancestors id pushes)))))

(defn flatten [ee] (flat-map identity ee))

(extend-protocol Monoid
  RawEvent
  (mappend [e1 e2] (join e1 e2))
  (maplus [e1 e2] (join e1 e2))
  (mempty [_] never))
