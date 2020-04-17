(ns mayu.frp.event
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

(defrecord RawEvent [atom])

(def next-event-id (atom 1))

(def never (->RawEvent (atom {:id 0 :on? true})))

(defn never? [e] (= 0 (:id @(:atom e))))

(defrecord Val [val ancestors])
(defrecord Ancestors [ancestors])

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

(defprotomethod val? [_] Val true Ancestors false)

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

(defn push! [e val] (send! e (->Val val {})))

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

(defn consume! [e on] (subscribe! e (fn [msg]
                                      (when (val? msg) (on (:val msg))))))

(defn shadow [e-src]
  (if (never? e-src)
    never
    (on! (Event
          #(subscribe! e-src (fn [msg]
                               (when (val? msg)
                                 (% (assoc msg :ancestors {})))))))))

(defn map [f e-src]
  (if (never? e-src)
    never
    (let [{:keys [atom]} e-src
          {:keys [id ancestors pushes]} @atom]
      (on! (Event
            #(subscribe! e-src (fn [msg]
                                 (% (if (val? msg)
                                      (update msg :val f)
                                      msg))))
            (assoc ancestors id pushes))))))

(defn reduce [r i e-src]
  (if (never? e-src)
    never
    (let [a-acc (atom i)
          {:keys [atom]} e-src
          {:keys [id ancestors pushes]} @atom]
      (on! (Event
            #(subscribe! e-src (fn [{:keys [val] :as msg}]
                                 (if (val? msg)
                                   (do (swap! a-acc (fn [acc] (r acc val)))
                                       (% (assoc msg :val @a-acc)))
                                   (% msg))))
            (assoc ancestors id pushes))))))


(defn filter [p e-src]
  (if (never? e-src)
    never
    (let [{:keys [atom]} e-src
          {:keys [id ancestors pushes]} @atom
          ]
      (on! (Event
            #(subscribe! e-src
                         (fn [{:keys [val ancestors] :as msg}]
                           (cond (and (val? msg)
                                      (not (p val))) (% (->Ancestors ancestors))
                                 :else (% msg))))
            (assoc ancestors id pushes))))))

(defn to-ancestors [ancestor-data]
  (a/map-values (fn [id-data _] (apply min (-> id-data :counts vals)))
                @ancestor-data))

(defn process-message [ancestor-data send-self! e-src]
  (fn [msg]
    (let [{:keys [id]} @(:atom e-src)
          {:keys [type val ancestors]} msg
          awaiting (atom 1)

          dec-awaiting
          (fn [sent-sibling?]
            (swap! awaiting dec)
            (if (= 0 @awaiting)
              (do
                (send-self! (-> msg
                                (assoc :ancestors (to-ancestors ancestor-data))
                                (assoc :val {:sent-sibling? sent-sibling?
                                             :val val})))
                (or sent-sibling? (val? msg)))
              sent-sibling?))

          prev (to-ancestors ancestor-data)]
      (doseq [[anc-id count] ancestors]
        (swap! ancestor-data #(assoc-in %1 [anc-id :counts id] count)))
      (let [curr (to-ancestors ancestor-data)]
        (doseq [[anc-id count] ancestors]
          (cond
            (and (< (get prev anc-id) count)
                 (< (get curr anc-id) count))
            (do (swap! awaiting inc)
                (when (val? msg)
                  (swap! ancestor-data (curry update-in [anc-id :stashed]
                                              #(conj %1 {:dec dec-awaiting
                                                         :id id})))))
            (< (get prev anc-id) count)
            (do (core/reduce #((:dec %2) %1) (val? msg)
                             (get-in @ancestor-data [anc-id :stashed]))
                (swap! ancestor-data #(assoc-in %1 [anc-id :stashed] '()))))))
      (dec-awaiting false))))


(defn raw-join [& arg-events]
  (let [events (remove never? arg-events)

        on-required
        (fn [events ancestor-data send-self!]
          (let [offs (-> #(subscribe! %1 (process-message ancestor-data
                                                          send-self!
                                                          %1))
                         (core/map events)
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
                ancestor-data (atom init-ancestor-data)]
            (on! (Event #(on-required events ancestor-data %1)
                        (to-ancestors ancestor-data)))))]

    (case (count events)
      0 never
      1 (map #(-> {:val %1 :sent-sibling? false}) (first events))
      (perform-join events))))

(defn join [& events] (map :val (apply raw-join events)))

(defn join-skip-siblings [& events]
  (->> (apply raw-join events)
       (filter (comp not :sent-sibling?))
       (map :val)))

(defn preempt
  ([f] (preempt identity f))
  ([from-res f]
   (let [e-channel (chan)
         off-channel (chan)
         e-init (Event (fn [send-self!]
                         (go (let [{:keys [e quick-off]} (<! e-channel)
                                   {:keys [ancestors]} @(:atom e)
                                   off (subscribe! e send-self!)]
                               (on! e)
                               (quick-off)
                               (>! off-channel off)))
                         (fn [] (go ((<! off-channel))))))
         res (f e-init)
         e (from-res res)
         ;; TODO this is a hack but Im not sure why
         quick-off (consume! e (fn [_]))
         atom-init (:atom e-init)
         {:keys [ancestors]} @(:atom e)]
     (swap! atom-init #(assoc %1 :ancestors ancestors))
     (on! e-init)
     (swap! (:atom e) #(merge %1 {:awaiting-on (chan)
                                  :on? false}))
     (go (>! e-channel {:e e :quick-off quick-off}))
     res)))

(defn timer [ms]
  (let [c (chan 1)]
    (on! (Event #(do (go (>! c :on))
                     (go-loop []
                       (let [msg (<! c)]
                         (when (not= :off msg)
                           (%1 (->Val :on {}))
                           (<! (timeout ms))
                           (>! c :on)
                           (recur))))
                     (fn [] (go (>! c :off))))))))

(defn raw-flat-map [f e]
  (if (never? e)
    never
    (let [curr-off (atom (fn []))
          full-off (atom (fn []))
          curr-id (atom nil)
          {:keys [id ancestors pushes]} @(:atom e)
          init-ancestor-data (reduce-kv
                              (fn [data anc-id pushes]
                                (assoc-in data [anc-id :counts id] pushes))
                              {}
                              (assoc ancestors id pushes))
          ancestor-data (atom init-ancestor-data)

          process-val
          (fn [send-self! {:keys [val] :as msg}]
            (let [e-next (f val)
                  next @(:atom e-next)
                  next-id (:id next)]
              (doseq [[anc-id data] @ancestor-data]
                (swap! ancestor-data (fn [curr-data]
                                       (-> curr-data
                                           (update-in [anc-id :stashed]
                                                      (partial remove
                                                               #(= @curr-id
                                                                   (:id %1))))
                                           (update-in [anc-id :counts]
                                                      #(dissoc %1 @curr-id))))))
              (doseq [[anc-id pushes] (:ancestors next)]
                (swap! ancestor-data #(assoc-in %1 [anc-id :counts next-id]
                                                pushes)))
              (reset! curr-id next-id)
              (@curr-off)
              ((process-message ancestor-data send-self! e)
               (->Ancestors (:ancestors msg)))
              (reset! curr-off
                      (subscribe! e-next
                                  (process-message ancestor-data
                                                   send-self!
                                                   e-next)))))]
      (on! (Event
            (fn [send-self!]
              (reset! full-off
                      (subscribe! e (fn [msg]
                                      (if (val? msg)
                                        (process-val send-self! msg)
                                        ((process-message ancestor-data
                                                          send-self!
                                                          e) msg)))))
              (fn []
                (@curr-off)
                (@full-off)
                (reset! curr-off (fn []))
                (reset! full-off (fn []))))
            (assoc ancestors id pushes))))))

(defn flat-map [f e] (map :val (raw-flat-map f e)))

(defn flatten [ee] (flat-map identity ee))

(defn defer-off
  ([e] (defer-off e 0))
  ([e ms]
   (if (never? e)
     never
     (let [soft-on? (atom false)
           on? (atom false)
           off-fn (atom (fn []))]
       (on! (Event (fn [send-self!]
                     (reset! soft-on? true)
                     (when (not @on?)
                       (reset! off-fn (subscribe! e send-self!))
                       (reset! on? true))
                     (fn []
                       (reset! soft-on? false)
                       (go (<! (timeout ms))
                           (when (not @soft-on?)
                             (reset! on? false)
                             (@off-fn)
                             (reset! off-fn (fn []))))))))))))

(defn throttle [e ms]
  (let [sender (atom (fn [_]))
        send! (fn [{:keys [val]}] (@sender (->Val val {})))
        throttling (atom false)
        queued (atom [])]
    (on!
     (Event (fn [send-self!]
              (reset! sender send-self!)
              (let [off
                    (subscribe! e
                                (fn [msg]
                                  (when (val? msg)
                                    (if @throttling
                                      (reset! queued [msg])
                                      (do (reset! throttling true)
                                          (send! msg)
                                          (go-loop []
                                            (<! (timeout ms))
                                            (if (> (count @queued) 0)
                                              (do (send! (first @queued))
                                                  (reset! queued [])
                                                  (recur))
                                              (reset! throttling false))))))))]
                (fn [] (off) (reset! sender (fn [_])))))))))

(extend-protocol Monoid
  RawEvent
  (mappend [e1 e2] (join e1 e2))
  (maplus [e1 e2] (join e1 e2))
  (mempty [_] never))
