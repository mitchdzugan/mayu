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

(defprotomethod to-vdoms [mdom e-el]
  !mdom/MText
  [(:s mdom)]

  !mdom/MCreateElement
  (let [{:keys [tag key path attrs children]} mdom
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
                  :on {:input #(let [target (aget %1 "target")]
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
                                  0)
                                 )}
                  :hook {:insert #(e/push! e-el {:path path
                                                 :el (aget %1 "elm")})
                         :postpatch #(e/push! e-el {:path path
                                                    :el (aget %1 "elm")})}}
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
                                                    v))))))))
                 clj->js)
        vchildren (clj->js (mapcat (curry to-vdoms e-el) children))]
    [(h tag data vchildren)])

  !mdom/MBind
  (mapcat (curry to-vdoms e-el) (s/inst! (:signal mdom)))

  !mdom/MSSRAwait
  (mapcat (curry to-vdoms e-el) (:children mdom)))

(defn attach [id raw-env ui]
  (let [e-el (e/on! (e/Event))
        env (merge raw-env {:e-el e-el})
        a-prev (atom (.getElementById js/document id))
        patch (init #js [(.-default attrs)
                         (.-default class)
                         (.-default el)
                         #js {:create update-mutable
                              :update update-mutable}
                         (.-default style)])]
    (dom/run e-el false env ui
      #(let [vdom (h "div" (clj->js {:attrs {:id id}})
                     (clj->js (mapcat (curry to-vdoms e-el) %1)))]
         (patch @a-prev vdom)
         (reset! a-prev vdom)))))

