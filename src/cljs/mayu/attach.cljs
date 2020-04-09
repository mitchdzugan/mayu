(ns mayu.attach
  (:require [allpa.core
             :refer [defn-match]]
            [mayu.dom :as dom]
            [mayu.mdom
             :refer [MText MCreateElement MBind]]
            [snabbdom :refer [init h]]
            [snabbdom.modules.attributes :as attrs]
            [snabbdom.modules.class :as class]
            [snabbdom.modules.eventlisteners :as el]
            [snabbdom.modules.style :as style]))

(defn-match to-vdoms
  [(MText s)]
  [s]

  [(MCreateElement tag attrs children)]
  [(h tag (clj->js {:attrs attrs}) (clj->js (mapcat to-vdoms children)))]

  [(MBind)]
  (throw "unimplemented"))

(defn attach [id env ui]
  (let [a-prev (atom (.getElementById js/document id))
        patch (init #js [(.-default attrs)
                                     (.-default class)
                                     (.-default el)
                                     (.-default style)])]
    (dom/run env ui
      #(let [vdom (h "div" (clj->js {:attrs {:id id}})
                     (clj->js (mapcat to-vdoms %1)))]
         (patch @a-prev vdom)
         (reset! a-prev vdom)))))

