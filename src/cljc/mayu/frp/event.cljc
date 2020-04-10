(ns mayu.frp.event
  (:require [allpa.core :as a
             :refer [curry deftagged defn-match fn-match match]]
            [mayu.async
             :refer [go go-loop timeout <! chan >!]]
            [wayra.monoid
             :refer [Monoid mappend maplus]]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])))

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
                        :awaiting-on (chan)})))))

(defn-match val?
  [(Val)] true
  [_] false)

(defn send! [e msg]
  (if (never? e)
    nil
    (let [{:keys [atom]} e
          {:keys [type val] merge-ancestors :ancestors} msg
          {:keys [id pushes ancestors subs on? awaiting-on]} @atom
          new-pushes (if (val? msg) (inc pushes) pushes)
          new-ancestors (merge ancestors merge-ancestors)]
      (if on?
        (do
          (when (contains? ancestors id)
            (throw (a/Err "Resursive Event Encountered")))
          (swap! atom (curry merge {:pushes new-pushes
                                    :ancestors new-ancestors}))
          (doseq [on (vals subs)]
            (on (assoc-in msg [:ancestors id] new-pushes))))
        (go (>! awaiting-on msg))))))

(defn on! [e]
  (let [{:keys [atom]} e
        {:keys [on? awaiting-on]} @atom]
    (when (not on?)
      (swap! atom (curry assoc :on? true))
      (go-loop []
        (let [msg (<! awaiting-on)]
          (when msg
            (send! e msg)
            (recur)))))
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
                      (swap! ancestor-data (curry update-in [anc-id :stashed]
                                                  #(conj %1 dec-awaiting))))
                  (< (get prev anc-id) count)
                  (do (reduce #(%2 %1) (val? msg) (get-in @ancestor-data
                                                          [anc-id :stashed]))
                      (swap! ancestor-data #(assoc-in %1 [anc-id :stashed]
                                                      '()))))))
            (dec-awaiting false)))

        on-required
        (fn [events ancestor-data to-ancestors send-self!]
          (let [offs
                (->> events
                     (map (fn [event]
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
                (reduce
                 (fn [data event]
                   (let [atom (:atom event)
                         {:keys [id ancestors]} @atom]
                     (reduce-kv
                      (fn [data anc-id pushes]
                        (assoc-in data [anc-id :counts id] pushes))
                      data
                      ancestors)))
                 {}
                 events)
                ancestor-data (atom init-ancestor-data)
                to-ancestors (fn []
                               (a/map-values #(apply min (-> %1 :counts vals))
                                             @ancestor-data))]
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

(extend-protocol Monoid
  RawEvent
  (mappend [e1 e2] (join e1 e2))
  (maplus [e1 e2] (join e1 e2)))
