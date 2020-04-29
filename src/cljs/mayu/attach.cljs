(ns mayu.attach
  (:require [clojure.string :as str]
            [allpa.core :as a
             :refer [curry defprotomethod]]
            [mayu.dom :as dom]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.mdom :as mdom]
            ["snabbdom" :refer [init h]]
            ["snabbdom/modules/attributes" :as attrs]
            ["snabbdom/modules/class" :as class]
            ["snabbdom/modules/eventlisteners" :as el]
            ["snabbdom/modules/style" :as style]))

(def mutable-keys [:value :checked :selected])

(defn update-mutable [prev curr]
  (let [elm (aget curr "elm")
        data (or (aget curr "data") #js {})
        mutable (or (aget data "mutable") #js {})]
    (doseq [kw-key mutable-keys]
      (let [key (name kw-key)
            val (aget mutable key)
            mkey (str "_mayu_mutable_" key)]
        (when (and val (not= val (aget elm key)))
          (aset elm key val))
        (when (and val (not= val (aget elm mkey)))
          (aset elm mkey val))))))

(defrecord TText [s])
(defrecord TCreateElement [tag key path attrs children])

(def ^:dynamic g-render-info (atom {}))

(def on-input
  #(let [target (aget %1 "target")]
     (js/setTimeout
      (fn []
        (doseq [kw-key mutable-keys]
          (let [key (name kw-key)
                mkey (str "_mayu_mutable_" key)
                mval (aget target mkey)
                val (get target key)]
            (when (and mval
                       (not= val mval))
              (aset target key mval)))))
      0)))

(defn push-el [vnode]
  (let [el (aget vnode "elm")
        data (or (aget vnode "data") #js{})
        path (aget data "path")]
    (swap! g-render-info #(-> %1
                              (assoc-in [:els path] el)
                              (assoc-in [:rendered path] true)))))

(declare thunk)

(defprotomethod to-vdoms [tdom]
  TText
  (:s tdom)

  TCreateElement
  (let [{:keys [tag key path attrs children]} tdom
        mutable (reduce #(if (contains? attrs %2)
                           (assoc %1 %2 (get attrs %2))
                           %1)
                        {}
                        mutable-keys)
        fixed-attrs (reduce #(dissoc %1 %2)
                            (dissoc attrs :style)
                            mutable-keys)
        fix-keys
        (fn [styles]
          (a/map-keys (fn [_ k]
                        (->> (name k)
                             (reduce (fn [{:keys [up? s]} c]
                                       (cond
                                         up? {:up? false
                                              :s (str s (str/upper-case c))}
                                         (= \- c) {:up? true :s s}
                                         :else {:up? false :s (str s c)}))
                                     {:up? false :s ""})
                             :s))
                      styles))

        data (-> {:attrs fixed-attrs
                  :mutable mutable
                  :path path
                  :on {:input on-input}
                  :hook {:insert push-el
                         :postpatch push-el}}
                 (#(if (nil? key) %1 (assoc %1 :key key)))
                 ((fn [data]
                    (if (empty? (:style attrs))
                      data
                      (assoc data :style
                             (->> (:style attrs)
                                  fix-keys
                                  (a/map-values (fn [v _]
                                                  (if (map? v)
                                                    (fix-keys v)
                                                    v)))))))))]
    (thunk path tag data children)))

(defn build-thunk [thunk]
  (let [data (or (aget thunk "data") #js {})
        [tag data children] (or (aget data "args") [])
        jsdata (clj->js data)]
    (aset jsdata "path" (:path data))
    (h tag jsdata (clj->js (map to-vdoms children)))))

(defn copy-to-thunk [vnode thunk]
  (let [thunk-data (or (aget thunk "data") #js {})
        vnode-data (or (aget vnode "data") #js {})]
    (aset vnode-data "args" (aget thunk-data "args"))
    (aset vnode "data" vnode-data)
    (aset thunk "data" (aget vnode "data"))
    (aset thunk "children" (aget vnode "children"))
    (aset thunk "text" (aget vnode "text"))
    (aset thunk "elm" (aget vnode "elm"))))

(defn thunk-init [thunk]
  (let [vnode (build-thunk thunk)]
    (copy-to-thunk vnode thunk)))

(defn thunk-prepatch [prev curr]
  (let [prev-data (or (aget prev "data") #js {})
        curr-data (or (aget curr "data") #js {})
        prev-args (or (aget prev-data "args") #js {})
        curr-args (or (aget curr-data "args") #js {})]
    (copy-to-thunk (if (= prev-args curr-args) prev (build-thunk curr)) curr)))

(defn thunk [path tag data children]
  (let [jsdata #js {:hook #js {:init thunk-init :prepatch thunk-prepatch}
                    :args [tag data children]}]
    (when (not (nil? (:key data)))
      (aset jsdata "key" (:key data)))
    (h tag jsdata)))

(defprotomethod to-tdoms [mdom]
  !mdom/MText
  [(->TText (:s mdom))]

  !mdom/MCreateElement
  (let [{:keys [tag key path attrs children]} mdom]
    [(->TCreateElement tag key path attrs (mapcat to-tdoms children))])

  !mdom/MBind
  (mapcat to-tdoms (s/inst! (:signal mdom)))

  !mdom/MSSRAwait
  (mapcat to-tdoms (:children mdom)))

(defn post-render [e-render-info render-info]
  (fn []
    (e/push! e-render-info @render-info)))

(defn attach [id env ui]
  (let [render-info (atom {})
        e-render-info (e/on! (e/Event))
        a-prev (atom (.getElementById js/document id))
        patch (init #js [(.-default attrs)
                         (.-default class)
                         (.-default el)
                         #js {:create update-mutable
                              :update update-mutable
                              :post (post-render e-render-info render-info)}
                         (.-default style)])]
    (dom/run e-render-info false env ui
      #(binding [g-render-info render-info]
         (let [vdom (h "div" (clj->js {:attrs {:id id}})
                       (clj->js (->> %1
                                     (mapcat to-tdoms)
                                     (map to-vdoms))))]
           (patch @a-prev vdom)
           (reset! a-prev vdom))))))

