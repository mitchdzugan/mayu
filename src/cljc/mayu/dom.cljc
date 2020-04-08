(ns mayu.dom
  (:require [allpa.core
             :refer [curry]]
            [mayu.mdom
             :refer [MText MCreateElement MBind]]
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

(defnm text [s] (w/tell {:mdom (MText s)}))

(defnm create-element
  ([tag] (create-element tag {} (w/pure nil)))
  ([tag arg]
   (let [[attrs m] (if (map? arg)
                     [arg (w/pure nil)]
                     [{} arg])]
     (create-element tag attrs m)))
  ([tag attrs m-]
   (let [m (if (string? m-) (text m-) m-)]
     (w/pass (mdo res <- m
                  [[res
                    (curry update :mdom
                           #(-> [(MCreateElement tag attrs %1)]))]])))))

(defnm emit [k e] (w/tell {:events {k e}}))

;; TODO need preempt implemented
(defnm collect [])

;; TODO need frp/signals implemented
(defnm use [])

;; TODO need frp/signals implemented
(defnm bind [])

;; TODO unsure of implementation details
(defnm memo [])

;; TODO unsure of implementation details
(defnm keyed [])

;; TODO need use/collect/signals implemented
(defnm collect-and-reduce [])

(defnm stash-dom [m]
  (w/pass (mdo [res, w] <- (w/listen m)
               [[{:res res :mdom (:mdom w)}
                 (curry assoc :mdom [])]])))

(defnm apply-stash [{:keys [mdom]}] (w/eachm mdom #(w/tell {:mdom %1})))

(defn run [env m use-mdom]
  (->> m
       (w/exec {:reader {:env env}
                :init-writer {:mdom []}})
       :writer
       :mdom
       use-mdom))
