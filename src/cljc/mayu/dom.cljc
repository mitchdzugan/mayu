(ns mayu.dom
  (:require [allpa.core
             :refer [curry]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.mdom
             :refer [MText MCreateElement MBind]]
            [wayra.impl :as wimpl]
            [wayra.core :as w
             :refer [defnm defm mdo]]))

(def tags
  ["a" "abbr" "acronym" "address" "applet" "area" "article" "aside" "audio" "b"
   "base" "basefont" "bdo" "big" "blockquote" "body" "br" "button" "canvas"
   "caption" "center" "cite" "code" "col" "colgroup" "datalist" "dd" "del" "dfn"
   "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure" "font" "footer"
   "form" "frame" "frameset" "head" "header" "h1" "h2" "h3" "h4" "h5" "h6" "hr"
   "html" "i" "iframe" "img" "input" "ins" "kbd" "label" "legend" "li" "link"
   "main" "map" "mark" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup"
   "option" "p" "param" "pre" "progress" "q" "s" "samp" "script" "section"
   "select" "small" "source" "span" "strike" "strong" "style" "sub" "sup"
   "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "u"
   "ul" "var" "video" "wbr"])

(def tag-map (reduce #(assoc %1 %2 true) {} tags))

(defm env (w/asks :env))

(defnm envs [f]
  e <- env
  [(f e)])

(defnm assoc-env [k v m] (w/local #(assoc-in %1 [:env k] v) m))

(defnm assoc-in-env [ks v m] (w/local #(assoc-in %1 (concat [:env] ks) v) m))

(defnm update-env [k f m] (w/local #(update-in %1 [:env k] f) m))

(defnm update-in-env [ks f m] (w/local #(update-in %1 (concat [:env] ks) f) m))

(defnm text [& s] (w/tell {:mdom (MText (apply str s))}))

(defm curr-path
  {:keys [path last-unique-step]} <- w/ask
  [(conj path last-unique-step)])

(defm curr-unique-path
  {:keys [unique-path last-unique-step]} <- w/ask
  [(conj unique-path last-unique-step)])

(defnm step [label m]
  {:keys [key label-counts]} <- w/get
  {:keys [path unique-path last-step last-unique-step]} <- w/ask
  let [full-label (str label "." key)
       label-count (get label-counts full-label 0)
       full-unique-label (str full-label "." label-count)]
  (w/modify #(merge %1 {:key nil :label-counts {}}))
  res <- (w/local (comp #(update %1 :path (curry conj last-step))
                        #(update %1 :unique-path (curry conj last-unique-step))
                        #(assoc %1 :last-step full-label)
                        #(assoc %1 :last-unique-step full-unique-label))
                  m)
  (w/modify #(merge %1 {:key nil :label-counts (assoc label-counts
                                                      full-label
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
                                                (%1 (e/Val dom-event)))]
                                  (.addEventListener el target handler)
                                  (fn []
                                    (.removeEventListener el target handler)))
                               e/Event
                               e/on!
                               e/defer-off)]
                       (aset el "__mayu_events" (assoc existing target res))
                       res)))))))]
  [[{:res res :make-event-from-target make-event-from-target}
    (curry update :mdom #(-> [(MCreateElement tag key path attrs %1)]))]])

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

(defnm emit [k e] (w/tell {:events {k e}}))

(defnm collect [k mf]
  (w/pass #(assoc-in %1 [:events k] e/never)
          (mdo {:keys [res]} <- (w/preemptm e/preempt :e
                                            #(mdo [res w] <- (w/listen (mf %1))
                                                  let [e (get-in w [:events k])]
                                                  [{:res res :e e}]))
               [[res (curry update :events #(dissoc %1 k))]])))

;; TODO need frp/signals implemented
(defnm s-use [])

(def log #?(:clj println :cljs #(.log js/console %1)))

;; TODO need frp/signals implemented
(defnm bind [s f]
  (step "bind"
   (mdo
    {:keys [init-writer reader state]} <- wimpl/raw-get
    let [{:keys [signal]}
         (s/build (s/map #(w/exec {:init-writer (w/mempty init-writer)
                                   :reader reader}
                                  (f %1))
                         s))]
    [(s/consume! signal #(log (-> %1 :writer :mdom)))])))

;; TODO unsure of implementation details
(defnm memo [])

(defnm keyed [k] (w/modify #(merge %1 {:key k})))

;; TODO need use/collect/signals implemented
(defnm collect-and-reduce [])

(defnm stash [m]
  (w/pass (mdo [res w] <- (w/listen m)
               [[{:res res :mdom (:mdom w)}
                 (curry assoc :mdom [])]])))

(defnm unstash [{:keys [mdom]}] (w/eachm mdom #(w/tell {:mdom %1})))

(defn run [e-el env m use-mdom]
  (->> m
       (w/exec {:reader {:e-el e-el
                         :env env
                         :last-step ::root
                         :last-unique-step ::root}
                :init-writer {:mdom [] :events {}}})
       :writer
       :mdom
       use-mdom))
