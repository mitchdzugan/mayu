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

(def ^:dynamic g-render-info (atom {}))
(def ^:dynamic g-mutable-els (atom {}))
(def ^:dynamic g-post-render (atom []))

(defn update-mutable [prev curr]
  (let [has? (atom false)
        elm (aget curr "elm")
        prev-data (or (aget prev "data") #js {})
        curr-data (or (aget curr "data") #js {})
        curr-mutable (or (aget curr-data "mutable") #js {})
        prev-mutable (or (aget prev-data "mutable") #js {})
        path (aget curr-data "path")
        has-render? (not (nil? (aget curr-data "on-render")))
        on-render (or (aget curr-data "on-render") (fn []))]
    (swap! g-post-render (curry conj #(on-render elm)))
    (doseq [kw-key dom/mutable-keys]
      (let [key (name kw-key)
            val (aget curr-mutable key)]
        (when (.hasOwnProperty curr-mutable key)
          (reset! has? true)
          (when (not (.hasOwnProperty prev-mutable key))
            (aset elm "__mayu_path" path)
            (aset elm key val)))))
    (when @has?
      (swap! g-mutable-els #(assoc % path elm)))))

(defn handle-delayed [prev curr]
  (let [has? (atom false)
        elm (aget curr "elm")
        data (or (aget curr "data") #js {})
        args (or (aget data "args") [])
        [_ clj-data _ path] args]
    (->> (update-in args [1 :attrs] #(merge %1 (:delayed clj-data)))
         (aset data "args"))))

(defrecord TText [s])
(defrecord TCreateElement [tag key path attrs children])

(defn push-el [vnode]
  (let [el (aget vnode "elm")
        data (or (aget vnode "data") #js{})
        path (aget data "path")]
    (swap! g-render-info #(assoc-in %1 [:els path] el))))

(defn push-el-mount [vnode]
  (let [el (aget vnode "elm")
        data (or (aget vnode "data") #js{})
        path (aget data "path")]
    (swap! g-render-info #(-> %1
                              (assoc-in [:mounted path] el)
                              (assoc-in [:els path] el)))))

(declare thunk)

(defn before-input [event]
  (let [target (aget event "target")
        value (or (aget target "value") "")
        path (aget target "__mayu_path")
        set? (aget target "__mayu_set?")]
    (when (and path (not= false set?))
      (aset target "__mayu_last" value)
      (aset target "__mayu_set?" false))))

(defn after-input [elm event]
  (let [target (aget event "target")
        path (aget target "__mayu_path")
        last (aget target "__mayu_last")
        set? (aget target "__mayu_set?")
        buffered (or (aget target "__mayu_buffered_input") (atom a/queue))]
    #_(when (and last (= elm target) path (not set?) (empty? @buffered))
      (aset target "value" last))))

(defn add-after-input [prev curr]
  (let [elm (aget curr "elm")]
    (.addEventListener elm
                       "input"
                       (partial after-input elm)
                       #js{:capture false})))

(defprotomethod to-vdoms [tdom]
  TText
  (:s tdom)

  TCreateElement
  (let [{:keys [tag key path attrs children]} tdom
        {:keys [delayed on-render]} attrs
        mutable (reduce #(if (contains? attrs %2)
                           (assoc %1 %2 (get attrs %2))
                           %1)
                        {}
                        dom/mutable-keys)
        fixed-attrs (reduce #(dissoc %1 %2)
                            (dissoc attrs :style :delayed :on-render)
                            dom/mutable-keys)
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
                  :delayed delayed
                  :on-render on-render
                  :path path
                  :on {:beforeinput before-input}
                  :hook {:insert push-el-mount
                         :postpatch push-el}}
                 (#(if (nil? key) %1 (assoc %1 :key (str (hash key)))))
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
        prev-args (or (aget prev-data "args") [])
        curr-args (or (aget curr-data "args") [])
        [_ _ _ path] curr-args]
    (swap! g-render-info #(assoc-in %1 [:els path] (aget prev "elm")))
    (copy-to-thunk (if (= prev-args curr-args)
                     prev
                     (do
                       (handle-delayed prev curr)
                       (build-thunk curr)))
                   curr)))

(defn thunk [path tag data children]
  (let [jsdata #js {:hook #js {:init thunk-init
                               :prepatch thunk-prepatch
                               :insert push-el-mount
                               :postpatch push-el}
                    :args [tag data children path]}]
    (when (not (nil? (:key data)))
      (aset jsdata "key" (:key data)))
    (h tag jsdata)))

(defprotomethod to-tdoms [mdom]
  !mdom/MText
  [(->TText (:s mdom))]

  !mdom/MCreateElement
  (let [{:keys [tag key path attrs children]} mdom]
    (swap! g-render-info #(assoc-in %1 [:used path] true))
    [(->TCreateElement tag key path attrs (mapcat to-tdoms children))])

  !mdom/MBind
  (mapcat to-tdoms (s/inst! (:signal mdom)))

  !mdom/MSSRAwait
  (mapcat to-tdoms (:children mdom)))

(defn post-render [e-render-info render-info]
  (fn []
    (let [used (keys (:used @g-render-info))
          remove-unused #(select-keys % used)]
      (swap! g-render-info #(update % :els remove-unused))
      (swap! g-mutable-els remove-unused)
      (e/push! e-render-info @render-info)
      (swap! g-post-render (fn [fs] (doseq [f fs] (f)) [])))))

(defn attach [element env ui]
  (let [a-post-render (atom [])
        a-mutable-els (atom {})
        attrs? (.hasAttributes element)
        raw-attrs (if attrs?
                    (aget element "attributes")
                    #js[])
        attrs-map (->> (range (aget raw-attrs "length"))
                       (reduce #(assoc %1
                                       (aget (aget raw-attrs %2) "name")
                                       (aget (aget raw-attrs %2) "value"))
                               {}))
        render-info (atom {})
        e-render-info (e/on! (e/Event))
        a-prev (atom element)
        patch (init #js [(.-default attrs)
                         (.-default class)
                         (.-default el)
                         #js {:create add-after-input}
                         #js {:create update-mutable
                              :update update-mutable
                              :post (post-render e-render-info render-info)}
                         (.-default style)])]
    (dom/run {:a-mutable-els a-mutable-els
              :e-render-info e-render-info
              :to-js clj->js} env ui
      #(binding [g-post-render a-post-render
                 g-mutable-els a-mutable-els
                 g-render-info render-info]
         (swap! g-render-info (curry merge {:mounted {} :used {}}))
         (let [vdom (h (aget element "tagName")
                       (clj->js {:attrs attrs-map})
                       (clj->js (->> %1
                                     (mapcat to-tdoms)
                                     (map to-vdoms))))]
           (patch @a-prev vdom)
           (reset! a-prev vdom))))))

