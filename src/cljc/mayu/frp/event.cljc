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

(defn to-deps [dep-data]
  (a/map-values (fn [id-data _] (apply min (-> id-data :counts vals)))
                @dep-data))

(defn process-message [dep-data send-self! e-src]
  (fn [msg]
    (let [{:keys [id]} @(:state e-src)
          {:keys [type val deps]} msg
          awaiting (atom 1)

          dec-awaiting
          (fn [sent-sibling?]
            (swap! awaiting dec)
            (if (= 0 @awaiting)
              (do
                (send-self! (-> msg
                                (assoc :deps (to-deps dep-data))
                                (assoc :val {:sent-sibling? sent-sibling?
                                             :val val})))
                (or sent-sibling? (val? msg)))
              sent-sibling?))

          prev (to-deps dep-data)]
      (doseq [[dep-id count] deps]
        (swap! dep-data #(assoc-in %1 [dep-id :counts id] count)))
      (let [curr (to-deps dep-data)]
        (doseq [[dep-id count] deps]
          (cond
            (and (< (get prev dep-id count) count)
                 (< (get curr dep-id) count))
            (do (swap! awaiting inc)
                (when (val? msg)
                  (swap! dep-data (curry update-in [dep-id :stashed]
                                         #(conj %1 {:dec dec-awaiting
                                                    :id id})))))
            (< (get prev dep-id count) count)
            (do (core/reduce #((:dec %2) %1) (val? msg)
                             (get-in @dep-data [dep-id :stashed]))
                (swap! dep-data #(assoc-in %1 [dep-id :stashed] '()))))))
      (dec-awaiting (and (not (empty? deps))
                         (= prev (merge prev deps)))))))

(defn raw-join [& arg-events]
  (let [events (remove never? arg-events)

        on-required
        (fn [events dep-data send-self!]
          (let [offs (-> #(subscribe! %1 (process-message dep-data
                                                         send-self!
                                                         %1))
                         (core/map events)
                         vec)]
            (fn []
              (reset! dep-data {})
              (doseq [off offs] (off)))))

        perform-join
        (fn [events]
          (let [dep-data (atom {})]
            (on! (Event #(on-required events dep-data %1) false))))]

    (case (count events)
      0 never
      1 (map #(-> {:val %1 :sent-sibling? false}) (first events))
      (perform-join events))))

(defn join [& events] (map :val (apply raw-join events)))

(defn join-skip-siblings [& events]
  (->> (apply raw-join events)
       (filter (comp not :sent-sibling?))
       (map :val)))

(defn raw-flat-map [f e]
  (if (never? e)
    never
    (let [curr-off (atom (fn []))
          full-off (atom (fn []))
          curr-id (atom nil)
          {:keys [id pushes]} @(:state e)
          dep-data (atom {})

          process-val
          (fn [send-self! {:keys [val] :as msg}]
            (let [e-next (f val)
                  next @(:state e-next)
                  next-id (:id next)]
              (doseq [[dep-id data] @dep-data]
                (swap! dep-data (fn [curr-data]
                                  (-> curr-data
                                      (update-in [dep-id :stashed]
                                                 (partial remove
                                                          #(= @curr-id
                                                              (:id %1))))
                                      (update-in [dep-id :counts]
                                                 #(dissoc %1 @curr-id))))))
              (swap! dep-data #(into {} (core/filter (fn [[_ data]]
                                                       (not= 0 (+ (count (:counts data))
                                                                  (count (:stashed data)))))
                                                     %1)))
              (reset! curr-id next-id)
              (@curr-off)
              ((process-message dep-data send-self! e)
               (->Deps (:deps msg)))
              (reset! curr-off
                      (subscribe! e-next
                                  (process-message dep-data
                                                   send-self!
                                                   e-next)))))]
      (on! (Event
            (fn [send-self!]
              (reset! full-off
                      (subscribe! e (fn [msg]
                                      (if (val? msg)
                                        (process-val send-self! msg)
                                        ((process-message dep-data
                                                          send-self!
                                                          e) msg)))))
              (fn []
                (@curr-off)
                (@full-off)
                (reset! dep-data {})
                (reset! curr-off (fn []))
                (reset! full-off (fn []))))
            false)))))

(defn flat-map [f e] (map :val (raw-flat-map f e)))

(defn flatten [ee] (flat-map identity ee))

(defn on-preempted [init-id send-self!]
  (fn [msg]
    (when (not (nil? (get-in msg [:deps init-id])))
      (throw (a/Err "Resursive Event Encountered")))
    (send-self! msg)))

(defn preempt
  ([f] (preempt identity f))
  ([from-res f]
   (let [e-channel (chan)
         off-channel (chan)
         e-init (Event (fn [send-self!]
                         (go (let [{:keys [e quick-off init-id]} (<! e-channel)
                                   off (subscribe! e (on-preempted init-id
                                                                   send-self!))]
                               (on! e)
                               (quick-off)
                               (>! off-channel off)))
                         (fn [] (go ((<! off-channel)))))
                       false)
         res (f e-init)
         e (from-res res)
         ;; TODO this is a hack but Im not sure why
         quick-off (consume! e (fn [_]))]
     (on! e-init)
     (swap! (:state e) #(merge %1 {:awaiting-on (chan) :on? false}))
     (go (>! e-channel {:e e :quick-off quick-off}))
     res)))

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
                             (reset! off-fn (fn []))))))
                   false))))))

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
                (fn [] (off) (reset! sender (fn [_])))))
            false))))

(defn defer [e ms]
  (let [sender (atom (fn [_]))
        send! (fn [{:keys [val]}] (@sender (->Val val {})))]
    (on!
     (Event (fn [send-self!]
              (reset! sender send-self!)
              (let [off
                    (subscribe! e
                                (fn [msg]
                                  (when (val? msg)
                                    (go (<! (timeout ms))
                                        (send! msg)))))]
                (fn [] (off) (reset! sender (fn [_])))))
            false))))
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
                     (fn [] (go (>! c :off))))
                false))))

(extend-protocol Monoid
  RawEvent
  (mappend [e1 e2] (join e1 e2))
  (maplus [e1 e2] (join e1 e2))
  (mempty [_] never))
