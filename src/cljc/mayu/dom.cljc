(ns mayu.dom
  (:require [allpa.core :as a
             :refer [curry varg#]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.async
             :refer [go go-loop timeout <! chan >! close!]]
            [mayu.mdom
             :refer [->MText ->MCreateElement ->MBind]]
            [wayra.core :as w
             :refer [defnm defm mdo fnm]]))

(defm env (w/asks :env))

(defnm envs [f]
  e <- env
  [(f e)])

(defnm assoc-env [k v m] (w/local #(assoc-in %1 [:env k] v) m))

(defnm assoc-in-env [ks v m] (w/local #(assoc-in %1 (concat [:env] ks) v) m))

(defnm update-env [k f m] (w/local #(update-in %1 [:env k] f) m))

(defnm update-in-env [ks f m] (w/local #(update-in %1 (concat [:env] ks) f) m))

(defnm text [& s] (w/tell {:mdom (->MText (apply str s))}))

(defm curr-path
  {:keys [path last-unique-step]} <- w/ask
  [(conj path last-unique-step)])

(defm curr-unique-path
  {:keys [unique-path last-unique-step]} <- w/ask
  [(conj unique-path last-unique-step)])

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

(defn on-click [{:keys [make-event-from-target]}]
  (make-event-from-target "click"))

(defnm inner-create-element [key tag attrs m]
  path <- curr-unique-path
  res <- m
  {:keys [e-el]} <- w/ask
  let [make-event-from-target
       (fn [target]
         (->> e-el
              (e/filter #(= (:path %1) path))
              (e/map :el)
              e/shadow
              (e/flat-map
               (fn [el]
                 (let [existing (aget el "__mayu_events")]
                   (if (get existing target)
                     (get existing target)
                     (let [res
                           (-> #(let [handler (fn [dom-event]
                                                (%1 (e/->Val dom-event {})))]
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
  ([tag attrs m-]
   let [m (if (string? m-) (text m-) m-)]
   {:keys [key]} <- w/get
   (w/pass (step tag (inner-create-element key tag attrs m)))))

(defnm emit [k e]
  (w/tell {:events {k e}}))

(defnm collect [k mf]
  (w/pass #(assoc-in %1 [:events k] e/never)
          (mdo {:keys [res]} <- (w/preemptm e/preempt :e
                                            #(mdo [res w] <- (w/listen (mf %1))
                                                  let [e (get-in w [:events k])]
                                                  [{:res res :e e}]))
               [[res (curry update :events #(dissoc %1 k))]])))

(defn reset-used [m]
  (a/map-values (varg# (assoc %1 :used? false)) m))

(defn check-used [off]
  (fn [m]
    (let [reducer (fn [agg id item]
                    (if (:used? item)
                      (assoc agg id item)
                      (do (off item)
                          agg)))]
      (reduce-kv reducer {} m))))

(defn off-bind [{{:keys [memos signals binds]} :state :as bind}]
  (reset! memos {})
  (doseq [[_ {:keys [off]}] @signals]
    (off))
  (reset! signals {})
  (doseq [[_ bind] @binds]
    (off-bind bind))
  (reset! binds {}))

(defnm bind [s f]
  (step ::bind
        (mdo
         init-writer <- w/erased
         path <- curr-path
         {:keys [binds] :as reader} <- w/ask
         let [bind (or (get-in @binds [path :state])
                       {:memos (atom {})
                        :signals (atom {})
                        :binds (atom {})})]
         [(swap! binds #(-> %1
                            (assoc-in [path :used?] true)
                            (assoc-in [path :state] bind)))]
         s-exec <- (s/map #(do (swap! (:signals bind) reset-used)
                               (swap! (:binds bind) reset-used)
                               (let [res (->> (w/local (curry merge bind) (f %1))
                                              (w/exec {:init-writer init-writer
                                                       :reader reader}))]
                                 (swap! (:signals bind)
                                        (check-used (fn [{:keys [off]}] (off))))
                                 (swap! (:binds bind)
                                        (check-used off-bind))
                                 res))
                          s)
         s-writer <- (s/map :writer s-exec)
         s-events <- (s/map :events s-writer)
         s-result <- (s/map :result s-exec)
         s-mdom <- (s/map :mdom s-writer)
         (w/tell {:mdom (->MBind s-mdom)})
         (emit ::request-render (e/shadow (s/changed s-mdom)))
         (w/eachm (keys (:events init-writer))
                  #(step %1 (mdo s-event <- (s/map %1 s-events)
                                 (emit %1 (s/unwrap-event s-event)))))
         [s-result])))

(defnm memo [via m]
  (step ::memo
        (mdo memos <- (w/asks :memos)
             path <- curr-path
             [nil])))

(defnm keyed [k m]
  (step k (mdo (w/modify #(assoc %1 :key k))
               m)))

(defnm collect-and-reduce [k r i m]
  (collect k (fnm [e]
                  s <- (s/reduce r i e)
                  (assoc-env k s m))))

(defnm stash [m]
  (w/pass (mdo [res w] <- (w/listen m)
               [[{:res res :mdom (:mdom w)}
                 (curry assoc :mdom [])]])))

(defnm unstash [{:keys [mdom]}] (w/eachm mdom #(w/tell {:mdom %1})))

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

(defn run [e-el env m use-mdom]
  (let [{:keys [writer result]}
        (->> (collect ::request-render
                      (fnm [e]
                           result <- m
                           [{:result result
                             :request-render e}]))
             (w/exec {:reader {:e-el e-el
                               :step-fn step
                               :active-signal active-signal
                               :set-active set-active
                               :signals (atom {})
                               :binds (atom {})
                               :memos (atom {})
                               :env env
                               :last-step ::root
                               :last-unique-step ::root}
                      :init-writer {:mdom [] :events {}}}))
        off (e/consume! (e/throttle (:request-render result) 10)
                        (fn [_] (use-mdom (:mdom writer))))]
    (use-mdom (:mdom writer))
    {:off off :result (:result result)}))

(defnm when [b m]
  (w/whenm b m))
