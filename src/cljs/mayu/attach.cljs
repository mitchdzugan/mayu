(ns mayu.attach
  (:require [allpa.core
             :refer [defn-match fn-match]]
            [mayu.dom :as dom]
            [mayu.frp.event :as e]
            [mayu.mdom
             :refer [MText MCreateElement MBind]]
            ["snabbdom" :refer [init h]]
            ["snabbdom/modules/attributes" :as attrs]
            ["snabbdom/modules/class" :as class]
            ["snabbdom/modules/eventlisteners" :as el]
            ["snabbdom/modules/style" :as style]))

(defn to-vdoms [e-el]
  (fn-match
   [(MText s)]
   [s]

   [(MCreateElement tag key path attrs children)]
   (let [data (-> {:attrs attrs
                   :hook {:insert #(e/push! e-el {:path path
                                                  :el (.-elm %1)})
                          :postpatch #(e/push! e-el {:path path
                                                     :el (.-elm %1)})}}
                  (#(if (nil? key) %1 (assoc %1 :key key)))
                  clj->js)
         vchildren (clj->js (mapcat (to-vdoms e-el) children))]
     (.log js/console {:key key :path path})
     [(h tag data vchildren)])

   [(MBind)]
   (throw "unimplemented")))

(defn attach [id raw-env ui]
  (let [e-el (e/on! (e/Event))
        env (merge raw-env {:e-el e-el})
        a-prev (atom (.getElementById js/document id))
        patch (init #js [(.-default attrs)
                                     (.-default class)
                                     (.-default el)
                         (.-default style)])]
    ; (e/consume! e-el #(.log js/console %1))
    (dom/run e-el env ui
      #(let [vdom (h "div" (clj->js {:attrs {:id id}})
                     (clj->js (mapcat (to-vdoms e-el) %1)))]
         (patch @a-prev vdom)
         (reset! a-prev vdom)))))

