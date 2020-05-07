(ns mayu.dom
  (:require [allpa.core :as a
             :refer [curry defprotomethod]]
            [clojure.string :as str]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.async
             :refer [go go-loop timeout <! chan >! close!]]
            [mayu.dom.to-string.attrs
             :refer [render-attr-map]]
            [mayu.mdom
             :refer [->MText ->MCreateElement ->MBind ->MSSRAwait]]
            [wayra.core :as w
             :refer [defnm defm mdo fnm <#>]])
  #?(:cljs (:require-macros [mayu.dom :refer [mk-ons]])))

;; TODO investigate rapid binds probably has to do with mutually defined sigs

(defm env (w/asks :env))

(defnm envs [f]
  e <- env
  [(f e)])

(defnm assoc-env [k v m] (w/local #(assoc-in %1 [:env k] v) m))

(defnm assoc-in-env [ks v m] (w/local #(assoc-in %1 (concat [:env] ks) v) m))

(defnm update-env [k f m] (w/local #(update-in %1 [:env k] f) m))

(defnm update-in-env [ks f m] (w/local #(update-in %1 (concat [:env] ks) f) m))

(defnm text [& s] (w/tell {:mdom (->MText (apply str s))}))

(defm curr-unique-path
  {:keys [unique-path last-unique-step]} <- w/ask
  [(conj unique-path last-unique-step)])

;; TODO see if we can ever use non-unique path
(def curr-path curr-unique-path)
#_(defm curr-path
  {:keys [path last-unique-step]} <- w/ask
  [(conj path last-unique-step)])

(defnm step [label m]
  {:keys [label-counts]} <- w/get
  {:keys [path unique-path last-step last-unique-step]} <- w/ask
  let [label-count (get label-counts label 0)
       unique-label (str label "." label-count)]
  res <- (w/local (comp #(update %1 :path (curry conj last-step))
                        #(update %1 :unique-path (curry conj last-unique-step))
                        #(assoc %1 :last-step label)
                        #(assoc %1 :last-unique-step unique-label))
                  m)
  (w/modify #(merge %1 {:key nil
                        :label-counts (assoc label-counts
                                             label
                                             (inc label-count))}))
  [res])

(def targets
  ["on-abort" "on-after-print" "on-animation-end" "on-animation-iteration"
   "on-animation-start" "on-before-print" "on-before-unload" "on-blur"
   "on-can-play" "on-can-play-through" "on-change" "on-click" "on-context-menu"
   "on-copy" "on-cut" "on-dblclick" "on-drag" "on-drag-end" "on-drag-enter"
   "on-drag-leave" "on-drag-over" "on-drag-start" "on-drop" "on-duration-change"
   "on-ended" "on-error" "on-focus" "on-focus-in" "on-focus-out"
   "on-fullscreen-change" "on-fullscreen-error" "on-hash-change" "on-input"
   "on-invalid" "on-key-down" "on-key-press" "on-key-up" "on-load"
   "on-loaded-data" "on-loaded-metadata" "on-load-start" "on-message"
   "on-mouse-down" "on-mouse-enter" "on-mouse-leave" "on-mouse-move"
   "on-mouse-over" "on-mouse-out" "on-mouse-up" "on-mouse-wheel" "on-offline"
   "on-online" "on-open" "on-page-hide" "on-page-show" "on-paste" "on-pause"
   "on-play" "on-playing" "on-popstate" "on-progress" "on-rate-change"
   "on-resize" "on-reset" "on-scroll" "on-search" "on-seeked" "on-seeking"
   "on-select" "on-show" "on-stalled" "on-storage" "on-submit" "on-suspend"
   "on-time-update" "on-toggle" "on-touch-cancel" "on-touch-end" "on-touch-move"
   "on-touch-start" "on-transition-end" "on-unload" "on-volume-change"
   "on-waiting" "on-wheel"])

#?(:clj
   (defmacro mk-ons []
     `(do
        ~@(map (fn [name]
                 `(defn ~(symbol name) [el#]
                    ((:make-event-from-target el#)
                     ~(-> name (subs 3) (clojure.string/replace #"-" "")))))
               targets))))

(mk-ons)

(defm get-mount-event
  {:keys [parent-path e-render-info]} <- w/ask
  [(->> e-render-info
        e/shadow
        (e/map #(get-in %1 [:mounted parent-path]))
        (e/remove nil?))])

(defnm inner-create-element [key tag attrs m]
  path <- curr-unique-path
  res <- (w/local #(merge %1 {:parent-path path}) m)
  {:keys [e-render-info]} <- w/ask
  let [make-event-from-target
       (fn [target]
         (->> e-render-info
              e/shadow
              (e/map #(get-in %1 [:els path]))
              (e/remove nil?)
              e/dedup
              (e/flat-map
               (fn [el]
                 (let [existing (aget el "__mayu_events")]
                   (if (get existing target)
                     (get existing target)
                     (let [res
                           (-> #(let [handler (fn [dom-event]
                                                (%1 dom-event))]
                                  (.addEventListener el target handler)
                                  (fn []
                                     (.removeEventListener el target handler)))
                               e/Event
                               e/on!
                               e/defer-off)]
                       (aset el "__mayu_events" (assoc existing target res))
                       res)))))))]
  [[{:res res :make-event-from-target make-event-from-target}
    (curry update :mdom #(-> [(->MCreateElement tag key path attrs %1)]))]])

(defnm create-element
  ([tag] (create-element tag {} (w/pure nil)))
  ([tag arg]
   (let [[attrs m] (if (map? arg)
                     [arg (w/pure nil)]
                     [{} arg])]
     (create-element tag attrs m)))
  ([tag attrs- m-]
   let [to-single #(reduce str "" (interpose " " %))]
   let [attrs (if (contains? attrs- :class)
                (update attrs- :class
                        #(cond (map? %1) (->> (keys %1)
                                              (filter (partial get %1))
                                              (map name)
                                              to-single)
                               (coll? %1) (to-single %1)
                               :else %1))
                attrs-)
        m (if (fn? m-) m- (text m-))]
   {:keys [key]} <- w/get
   (w/pass (step tag (inner-create-element key tag attrs m)))))

(defnm create-element_ [& args]
  (<#> (apply create-element args)
       :res))

(defnm emit [k e]
  (w/tell {:events {k e}}))

(defnm collect [k mf]
  (step k
        (w/pass #(assoc-in %1 [:events k] [])
                (mdo {:keys [res]} <-
                     (w/preemptm e/preempt :e
                                 #(mdo [res w] <- (w/listen (mf %1))
                                       let [e (->> [:events k]
                                                   (get-in w)
                                                   (apply e/join))]
                                       [{:res res :e e}]))
                     [[res (curry update :events #(dissoc %1 k))]]))))

(defn reset-used [m]
  (a/map-values (fn [desc _] (assoc desc :used? false)) m))

(defn check-used [off]
  (fn [m]
    (let [reducer (fn [agg id item]
                    (if (:used? item)
                      (assoc agg id item)
                      (do (off item)
                          agg)))]
      (reduce-kv reducer {} m))))

(defn off-bind [{{:keys [memos signals binds offs]} :state :as bind}]
  (doseq [[_ memo] @memos]
    (off-bind memo))
  (reset! memos {})
  (doseq [off @offs]
    (off))
  (reset! offs [])
  (doseq [[_ {:keys [off]}] @signals]
    (off))
  (reset! signals {})
  (doseq [[_ bind] @binds]
    (off-bind bind))
  (reset! binds {}))

(defn pre-render [state]
  (swap! (:memos state) reset-used)
  (swap! (:signals state) reset-used)
  (swap! (:binds state) reset-used)
  (swap! (:offs state) (fn [offs]
                        (doseq [off offs]
                          (off))
                         [])))

(defn post-render [state]
  (swap! (:signals state)
         (check-used (fn [{:keys [off]}] (off))))
  (swap! (:binds state)
         (check-used off-bind))
  (swap! (:memos state)
         (check-used off-bind)))

(def bind-count (atom 0))
(defnm bind-base [s f proc-input]
  {:keys [e-request-render]} <- w/ask
  (step ::bind
        (mdo
         init-writer <- w/erased
         path <- curr-path
         {:keys [binds signals] :as reader} <- w/ask
         let [bind (or (get-in @binds [path :state])
                       {:memos (atom {})
                        :signals (atom {})
                        :binds (atom {})
                        :offs (atom [])})]
         [(swap! binds #(-> %1
                            (assoc-in [path :used?] true)
                            (assoc-in [path :state] bind)))]
         split-id <- (w/gets :split-id)
         s-split-id <- (s/from split-id e/never)
         s-shadowed <- (proc-input s)
         [(swap! bind-count inc)]
         (step @bind-count
               (mdo
                s-exec <-
                (s/map #(do (pre-render bind)
                            (let [split-id (s/inst! s-split-id)
                                  res (->> (w/local (curry merge bind) (f %1))
                                           (w/exec {:init-writer init-writer
                                                    :init-state {:split-id
                                                                 split-id}
                                                    :reader reader}))]
                              (post-render bind)
                              (e/push! e-request-render true)
                              res))
                       s-shadowed)
                (w/modify #(assoc %1 :split-id (get-in (s/inst! s-exec)
                                                       [:state :split-id])))
                s-writer <- (s/map :writer s-exec)
                s-events <- (s/map :events s-writer)
                s-result <- (s/map :result s-exec)
                s-mdom <- (s/map :mdom s-writer)
                (w/tell {:mdom (->MBind s-mdom)})
                (w/eachm (keys (:events init-writer))
                         #(step %1 (mdo s-event <-
                                        (s/map (fn [events]
                                                 (apply e/join (%1 events)))
                                               s-events)
                                        (->> s-event
                                             s/unwrap-event
                                             e/shadow
                                             (emit %1)))))
                [s-result])))))

;; TODO don't shadow input signal
;; TODO ideally this is the default version
;; TODO do performance tests to make sure its viable
(defn bind_ [s f]
  (bind-base s f #(w/pure %)))

(defn bind [s f]
  (bind-base s f s/shadow))

(defnm consume! [t f]
  let [event? (nil? (:inst! t))
       consumer (if event? e/consume! s/consume!)]
  offs <- (w/asks :offs)
  [(swap! offs (curry conj (consumer t f)))])

(defnm memo [via m]
  (step ::memo
        (mdo memos <- (w/asks :memos)
             path <- curr-path
             let [memo (get @memos path)
                  skip? (and memo (= (:via memo) via))
                  state (or (:state memo)
                            {:memos (atom {})
                             :signals (atom {})
                             :binds (atom {})
                             :offs (atom [])})]
             skip? --> (mdo (w/eachm (-> memo :writer :mdom)
                                     #(w/tell {:mdom %1}))
                            (w/eachm (keys (-> memo :writer :events))
                                     (fn [k]
                                       (w/eachm (-> memo :writer :events (get k))
                                                #(w/tell {:events {k %1}}))))
                            [(swap! memos #(assoc-in %1 [path :used?] true))
                             (:res memo)])
             [(pre-render state)]
             [res writer] <- (w/local (curry merge state)
                                      (w/listen m))
             [(post-render state)]
             [(swap! memos #(assoc %1 path {:used? true
                                            :via via
                                            :res res
                                            :writer writer
                                            :state state}))]
             [res])))

(defnm keyed [k m]
  (step k (mdo (w/modify #(assoc %1 :key k))
               m)))

(defnm collect-and-reduce [k r i m]
  (collect k (fnm [e]
                  s <- (s/reduce r i e)
                  (assoc-env k s m))))

(defnm collect-reduce-and-bind [k r i fm]
  (collect k (fnm [e]
                  s <- (s/reduce r i e)
                  (assoc-env k s (bind s fm)))))

(defn collect-values [k i m]
  (collect-and-reduce k #(-> %2) i m))

(defn collect-values-and-bind [k i fm]
  (collect-reduce-and-bind k #(-> %2) i fm))

(defnm stash [m]
  (w/pass (mdo [res w] <- (w/listen m)
               [[{:res res :mdom (:mdom w)}
                 (curry assoc :mdom [])]])))

(defnm unstash [{:keys [mdom]}] (w/eachm mdom #(w/tell {:mdom %1})))

(defnm ssr-await [ready? timeout fallback good]
  prev-split-path <- (w/asks :split-path)
  split-id <- (w/gets :split-id)
  let [split-path (conj prev-split-path split-id)]
  (w/modify #(update %1 :split-id inc))
  (step [::ssr-await split-path]
        (mdo
         ssr? <- (w/asks :ssr?)
         (not ssr?) --> (<#> good (fn [_] nil))
         s-timer <- (s/reduce (fn [curr _] (+ 100 curr))
                                0
                                (e/timer 100))
         (bind s-timer
               (fnm [timer]
                    (w/local #(assoc %1 :split-path split-path)
                             (w/pass (mdo
                                      let [timeout? (> timer timeout)
                                           done? (or ready? timeout?)
                                           fail? (and (not ready?)
                                                      timeout?)]
                                      (cond
                                        ready? good
                                        timeout? fallback
                                        :else (w/pure nil))
                                      [[nil
                                        (curry update :mdom
                                               #(-> [(->MSSRAwait split-path
                                                                  done?
                                                                  fail?
                                                                  %1)]))]])))))
         [])))

(defm active-signal
  path <- curr-path
  signals <- (w/asks :signals)
  [(get-in @signals [path :signal])])

(defnm set-active [signal]
  path <- curr-path
  signals <- (w/asks :signals)
  [(swap! signals #(assoc %1 path {:signal signal
                                   :off (fn [] (s/off! signal))
                                   :used? true}))])

(defn run [e-render-info ssr? env m use-mdom]
  (let [e-request-render (e/on! (e/Event))
        reader {:e-render-info e-render-info
                :e-request-render e-request-render
                :ssr? ssr?
                :step-fn step
                :active-signal active-signal
                :set-active set-active
                :signals (atom {})
                :binds (atom {})
                :memos (atom {})
                :offs (atom [])
                :split-path []
                :env env
                :last-step ::root
                :last-unique-step ::root}
        {:keys [writer result]}
        (->> m
             (w/exec {:reader reader
                      :init-state {:split-id 0}
                      :init-writer {:mdom [] :events {}}}))
        off (e/consume! (e/defer (e/throttle e-request-render 10) 0)
                        (fn [_] (use-mdom (:mdom writer))))]
    (use-mdom (:mdom writer))
    {:off (fn []
            (off-bind {:state reader})
            (off))
     :result result}))

(defn gen-uuid []
  (str (#?(:clj java.util.UUID/randomUUID :cljs random-uuid))))

(defprotomethod proc-mdom [mdom split-path state]
  !MSSRAwait
  (let [{:keys [uuids]} @state
        {next-path :split-path :keys [done? children]} mdom
        uuid (get uuids next-path (gen-uuid))]
    (when done?
      (swap! state #(update %1 :splits (curry conj [children next-path]))))
    (swap! state #(assoc-in %1 [:uuids next-path] uuid))
    (swap! state #(assoc-in %1 [:children split-path next-path] true))
    uuid)

  !MText
  (:s mdom)

  !MCreateElement
  (let [{:keys [tag attrs children]} mdom
        fixed-attrs (update attrs :style #(-> %1
                                              (dissoc :delayed :remove)
                                              (merge (:delayed %1))))]
    (str "<" tag (render-attr-map fixed-attrs) ">"
         (reduce #(str %1 (proc-mdom %2 split-path state)) "" children)
         "</" tag ">"))

  !MBind
  (reduce #(str %1 (proc-mdom %2 split-path state)) "" (s/inst! (:signal mdom))))

(defn str-split [s s1]
  (let [i (clojure.string/index-of s s1)]
    [(subs s 0 i) (subs s (+ i (count s1)))]))

(defn stream-rendered [split-path state send!]
  (cond
    (= -1 (last split-path)) true

    (nil? (get-in @state [:rendered split-path])) false

    :else
    (let [children (keys (get-in @state [:children split-path]))
          rendered (get-in @state [:rendered split-path])
          parts (->> children
                     (reduce (fn [{:keys [rest parts]} child-path]
                               (let [uuid (get-in @state [:uuids child-path])
                                     splits (str-split rest uuid)]
                                 {:rest (nth splits 1)
                                  :parts (conj parts (nth splits 0))}))
                             {:rest rendered :parts []})
                     (#(conj (:parts %1) (:rest %1))))]
      (->> (concat [(conj split-path -1)] children)
           (map (fn [post path] {:post post :path path}) parts)
           (reduce (fn [all? {:keys [path post]}]
                     (cond
                       (not all?) false
                       (get-in @state [:streamed path]) true

                       (stream-rendered path state send!)
                       (do (send! post)
                           (swap! state #(assoc-in %1 [:streamed path] true))
                           true)

                       :else false))
                   true)))))

(defn render-to-string [env m]
  (let [done? (atom false)
        off (atom (fn [] (reset! done? true)))
        state (atom {})
        send-id (atom 0)
        c (chan)
        send! #(let [id @send-id]
                 (swap! send-id inc)
                 (go (>! c [id %1])))
        fc (chan)

        {run-off :off}
        (run e/never true env m
          #(do (loop [[[mdoms split-path] & splits] [[%1 []]]]
                 (when-not (nil? split-path)
                   (swap! state (curry assoc :splits []))
                   (let [markup
                         (reduce (fn [agg mdom]
                                   (str agg (proc-mdom mdom split-path state)))
                                 ""
                                 mdoms)]
                     (when (nil? (get-in @state [:rendered split-path]))
                       (swap! state (curry assoc-in
                                           [:rendered split-path]
                                           markup)))
                     (recur (concat splits (:splits @state))))))
               (when (stream-rendered [] state send!)
                 (send! :off)
                 (@off)
                 )))]
    (if @done? (run-off) (reset! off run-off))
    (go-loop [off? false buffered {} next 0]
      (if (or (not off?) (not (empty? buffered)))
        (let [[id msg] (<! c)]
          (cond
            (= :off msg) (recur true buffered next)
            (not= id next) (recur off? (assoc buffered id msg) next)
            :else
            (do (>! fc msg)
                (let [[buffered next]
                      (loop [buffered buffered next (inc next)]
                        (if (contains? buffered next)
                          (do (>! fc (get buffered next))
                              (recur (dissoc buffered next) (inc next)))
                          [buffered next]))]
                  (recur off? buffered next)))))
        (do (close! c)
            (close! fc))))
    fc))
