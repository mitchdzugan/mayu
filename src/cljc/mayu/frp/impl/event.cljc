(ns mayu.frp.impl.event
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

(defrecord Push [val src count])
(defrecord Src [src count])
(defrecord Deps [deps])

(def global-push-counts (atom {}))

(defn global-count [id]
  (get @global-push-counts id 0))

(defn init-dep-counts [deps]
  (reduce #(assoc %1 %2 (global-count %2)) {} deps))

(defrecord RawEvent [state])

(def next-event-id (atom 1))

(def never (->RawEvent (atom {:id 0 :on? true})))

(defn never? [e] (= 0 (:id @(:state e))))

(defn Event
  ([] (Event (fn [_] (fn [])) nil))
  ([on-required] (Event on-required nil))
  ([on-required get-deps]
   (let [id @next-event-id]
     (swap! next-event-id inc)
     (->RawEvent (atom {:id id
                        :subs {}
                        :next-sub-id 1
                        :on-required on-required
                        :off-callback (fn [])
                        :source? (nil? get-deps)
                        :get-deps (or get-deps #(-> [id]))
                        :deps []
                        :pushes 1
                        :on? false
                        :num-awaiting 0
                        :awaiting-on (chan)})))))

(defprotomethod push? [_] Push true [Src Deps] false)
(defprotomethod deps? [_] Deps true [Push Src] false)

(defn send! [e msg]
  (when (not (never? e))
    (let [{:keys [state]} e
          {:keys [subs on? num-awaiting awaiting-on deps id pushes]} @state
          {new-deps :deps :keys [src count]} msg
          original-src src
          src (if (= original-src ::self) id src)
          count (if (= original-src ::self) (inc pushes) count)
          msg (if (= original-src ::self)
                (merge msg {:src src :count count})
                msg)]
      (when (= original-src ::self)
        (when (global-count id)
          (swap! global-push-counts (curry assoc id (inc pushes))))
        (swap! state (curry assoc :pushes (inc pushes))))
      (if on?
        (when (or (and (not (deps? msg))
                       (= count (global-count src))
                       (or (not= src ::none)
                           (push? msg)))
                  (and (deps? msg)
                       (not= deps new-deps)))
          (when (deps? msg)
            (swap! state #(assoc %1 :deps new-deps)))
          (doseq [on (vals subs)]
            (on msg)))
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
    (let [{:keys [state]} e]
      (swap! state (curry merge {:id 0 :subs nil :awaiting-on nil})))))

(defn push! [e val]
  (let [{:keys [state]} e
        {:keys [id pushes source?]} @state
        new-pushes (inc pushes)]
    (when (not source?)
      (throw (a/Err "Can only push to source event types")))
    (when (global-count id)
      (swap! global-push-counts (curry assoc id new-pushes)))
    (swap! state (curry assoc :pushes new-pushes))
    (send! e (->Push val id new-pushes))))

(defn subscribe! [e on]
  (if (never? e)
    (fn [])
    (let [{:keys [state]} e
          {:keys [next-sub-id on-required get-deps id pushes]} @state
          sub-count (fn [] (count (:subs @state)))]
      (swap! state #(-> %1
                       (assoc :next-sub-id (inc next-sub-id))
                       (assoc-in [:subs next-sub-id] on)))
      (when (= 1 (sub-count))
        (swap! global-push-counts #(assoc %1 id pushes))
        (swap! state (curry assoc :off-callback (on-required #(send! e %1))))
        (swap! state (curry assoc :deps (get-deps))))
      (fn []
        (swap! state (curry update :subs #(dissoc %1 next-sub-id)))
        (when (= 0 (sub-count))
          (swap! global-push-counts #(dissoc %1 id))
          ((:off-callback @state)))))))

(defn consume! [e on] (subscribe! e (fn [msg]
                                      (when (push? msg) (on (:val msg))))))

(defn shadow [e]
  (if (never? e)
    never
    (on! (Event
          #(subscribe! e (fn [msg]
                           (when (push? msg)
                             (% (merge msg {:src ::none :count 0})))))
          #(-> [])))))

(defn map [f e]
  (if (never? e)
    never
    (on! (Event
          #(subscribe! e (fn [msg]
                           (% (if (push? msg)
                                (update msg :val f)
                                msg))))
          #(:deps @(:state e))))))

(defn reduce [r i e]
  (if (never? e)
    never
    (let [a-acc (atom i)]
      (on! (Event
            #(subscribe! e (fn [{:keys [val] :as msg}]
                             (if (push? msg)
                               (do (swap! a-acc (fn [acc] (r acc val)))
                                   (% (assoc msg :val @a-acc)))
                               (% msg))))
            #(:deps @(:state e)))))))

(defn filter [p e]
  (if (never? e)
    never
    (on! (Event
          #(subscribe! e
                       (fn [{:keys [val src count] :as msg}]
                         (cond (and (push? msg)
                                    (not (p val))) (% (->Src src count))
                               :else (% msg))))
          #(:deps @(:state e))))))

(defn join-to-deps [dep-data]
  (->> @dep-data vals (core/reduce merge {}) keys))

(defprotomethod on-join-message [msg dep-data stashes send-self! e]
  Deps
  (let [{:keys [deps]} msg
        {:keys [id]} @(:state e)
        dep-counts (init-dep-counts deps)]
    (doseq [[src count] dep-counts]
      (let [stashed (get-in @stashes [src count])]
        (when (and (not (empty? stashed))
                   (->> @dep-data
                        vals
                        (every? #(= (get %1 src count) count))))
          (swap! stashes #(dissoc %1 src))
          (core/reduce #(%2 %1) false stashed))))
    (swap! dep-data #(assoc %1 id dep-counts))
    (send-self! (->Deps (join-to-deps dep-data))))

  [Push Src]
  (let [{:keys [val src count]} msg
        {:keys [id]} @(:state e)]
    (if (= ::none src)
      (send-self! msg)
      (let [sendable? (->> (dissoc @dep-data id)
                           vals
                           (every? #(= (get %1 src count) count)))
            stashed (get-in @stashes [src count])]
        (swap! dep-data #(assoc-in %1 [id src] count))
        (cond
          sendable?
          (do (swap! stashes #(dissoc %1 src))
              (when (or (empty? stashed)
                        (push? msg))
                (send-self! (merge msg {:val {:sent-sibling? false
                                              :val val}})))
              (core/reduce #(%2 %1) (push? msg) stashed))

          (push? msg)
          (let [send! #(do (send-self! (merge msg {:val {:sent-sibling? %1
                                                         :val val}}))
                           true)]
            (swap! stashes #(update-in %1 [src count] (curry conj send!)))))))))

(defn raw-join [& arg-events]
  (let [events (remove never? arg-events)

        on-required
        (fn [events dep-data stashes send-self!]
          (let [offs
                (-> #(subscribe! %1 (curry on-join-message
                                           dep-data
                                           stashes
                                           send-self!
                                           %1))
                    (core/map events)
                    vec)]
            (fn []
              (reset! dep-data {})
              (reset! stashes {})
              (doseq [off offs] (off)))))

        perform-join
        (fn [events]
          (let [stashes (atom {})
                dep-data (atom {})]
            (on! (Event #(on-required events dep-data stashes %1)
                        (fn []
                          (reset! dep-data
                                  (core/reduce #(let [{:keys [id deps]}
                                                      @(:state %2)]
                                                  (assoc %1 id
                                                         (init-dep-counts deps)))
                                               {}
                                               events))
                          (join-to-deps dep-data))))))]
    (case (count events)
      0 never
      1 (map #(-> {:val %1 :sent-sibling? false}) (first events))
      (perform-join events))))

(defn join [& events] (map :val (apply raw-join events)))

(defn join-skip-siblings [& events]
  (->> (apply raw-join events)
       (filter (comp not :sent-sibling?))
       (map :val)))

(defn flat-map [f e]
  (if (never? e)
    never
    (let [curr-off (atom (fn []))
          full-off (atom (fn []))
          {:keys [id]} @(:state e)
          inner-deps (atom {})
          outer-deps (atom {})
          inner-stashes (atom {})
          outer-stashes (atom {})

          to-deps
          (fn [] (->> (merge @inner-deps @outer-deps)
                      keys))

          process-inner
          (fn [send-self! {:keys [val src count deps] :as msg}]
            (cond
              (= ::none src)
              (send-self! msg)

              (deps? msg)
              (do (reset! inner-deps (init-dep-counts deps))
                  (send-self! (->Deps (to-deps)))
                  (doseq [[_ stashes] @outer-stashes]
                    ((get stashes (apply max (keys stashes)))))
                  (reset! outer-stashes {}))

              :else
              (do (swap! inner-deps #(assoc %1 src count))
                  (cond
                    (= (get @outer-deps src count) count)
                    (let [stashed (get-in @outer-stashes [src count])]
                      (when (or (push? msg)
                                (empty? stashed))
                        (send-self! msg))
                      (doseq [send! stashed]
                        (send!))
                      (swap! outer-stashes #(dissoc %1 src)))

                    (push? msg)
                    (swap! inner-stashes
                           (curry update-in [src count]
                                  #(conj %1 (fn [] (send-self! msg)))))))))

          process-outer
          (fn [send-self! {:keys [val src count deps] :as msg}]
            (cond
              (push? msg)
              (let [e-next (f val)
                    send!
                    (fn []
                      (@curr-off)
                      (reset! curr-off
                              (subscribe! e-next #(process-inner send-self! %1)))
                      (reset! inner-deps
                              (init-dep-counts (:deps @(:state e-next))))
                      (when (empty? (get-in @inner-stashes [src count]))
                        (send-self! (->Src src count)))
                      (send-self! (->Deps (to-deps)))
                      (doseq [[_ stashes] @outer-stashes]
                        ((get stashes (apply max (keys stashes)))))
                      (reset! outer-stashes {}))]
                (when-not (= ::none src)
                  (swap! outer-deps #(assoc %1 src count)))
                (cond
                  (= (get @inner-deps src count) count)
                  (let [stashed (get-in @inner-stashes [src count])]
                    (send!)
                    (doseq [send! stashed]
                      (send!))
                    (swap! inner-stashes #(dissoc %1 src)))

                  :else
                  (swap! outer-stashes
                         (curry update-in [src count]
                                #(conj %1 send!)))))

              (deps? msg)
              (do (reset! outer-deps (init-dep-counts deps))
                  (send-self! (->Deps (to-deps)))
                  (doseq [[_ stashes] @inner-stashes]
                    ((get stashes (apply max (keys stashes)))))
                  (reset! inner-stashes {}))

              :else
              (when-not (= ::none src)
                (swap! outer-deps #(assoc %1 src count))
                (let [stashed (get-in @inner-stashes [src count])]
                  (when (= (get @inner-deps src count) count)
                    (swap! inner-stashes #(dissoc %1 src))
                    (when (empty? stashed)
                      (send-self! msg))
                    (doseq [send! stashed]
                      (send!)))))))]
      (on! (Event
            (fn [send-self!]
              (reset! full-off (subscribe! e #(process-outer send-self! %1)))
              (fn []
                (@curr-off)
                (@full-off)
                (reset! inner-deps {})
                (reset! outer-deps {})
                (reset! inner-stashes {})
                (reset! inner-stashes {})
                (reset! curr-off (fn []))
                (reset! full-off (fn []))))
            (fn []
              (reset! outer-deps (init-dep-counts (:deps @(:state e))))
              (to-deps)))))))

(defn flatten [ee] (flat-map identity ee))

(defn on-preempted [init-id send-self!]
  (fn [msg]
    (when (= init-id (get msg :src))
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
                       #(-> []))
         res (f e-init)
         e (from-res res)
         ;; TODO this is a hack but Im not sure why its needed
         quick-off (consume! e (fn [_]))]
     (on! e-init)
     (swap! (:state e) #(merge %1 {:awaiting-on (chan) :on? false}))
     (go (>! e-channel {:e e :quick-off quick-off
                        :init-id (:id @(:state e-init))}))
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
                   #(:deps @(:state e))))))))

(defn throttle [e ms]
  (let [sender (atom (fn [_]))
        send! (fn [{:keys [val]}] (@sender (->Push val ::none 0)))
        throttling (atom false)
        queued (atom [])]
    (on!
     (Event (fn [send-self!]
              (reset! sender send-self!)
              (let [off
                    (subscribe! e
                                (fn [msg]
                                  (when (push? msg)
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
            #(-> [])))))

(defn defer [e ms]
  (let [sender (atom (fn [_]))
        send! (fn [{:keys [val]}] (@sender (->Push val ::none 0)))]
    (on!
     (Event (fn [send-self!]
              (reset! sender send-self!)
              (let [off
                    (subscribe! e
                                (fn [msg]
                                  (when (push? msg)
                                    (go (<! (timeout ms))
                                        (send! msg)))))]
                (fn [] (off) (reset! sender (fn [_])))))
            #(-> [])))))

(defn timer [ms]
  (let [c (chan 1)]
    (on! (Event #(do (go (>! c :on))
                     (go-loop []
                       (let [msg (<! c)]
                         (when (not= :off msg)
                           (%1 (->Push :on ::none 0))
                           (<! (timeout ms))
                           (>! c :on)
                           (recur))))
                     (fn [] (go (>! c :off))))
                #(-> [])))))

(extend-protocol Monoid
  RawEvent
  (mappend [e1 e2] (join e1 e2))
  (maplus [e1 e2] (join e1 e2))
  (mempty [_] never))
