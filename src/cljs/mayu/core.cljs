(ns mayu.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [wayra.core :as w
    :refer [defnm defm mdo]]
   [mayu.util :as u]
   [mayu.frp :as frp]
   [mayu.dom :as dom
    :refer [text create-element env envs stash-dom apply-stash]]
   [cljs.pprint :refer [pprint]]
   ))

(defm ui
  stashed <- (stash-dom (mdo (text "was")
                             (text "out")
                             (text "of")
                             (text "order")))
  (text "You")
  (text "thought")
  (text "this")
  (apply-stash stashed)
  (create-element "div" {} "Hello")
  (create-element "div" "Hello")
  (create-element "div")
  (create-element "img" {:src "htpps://....png"})
  )

(defn run-frp []
  (let [e0 (frp/Event)
        e1 (frp/fmap inc e0)
        e2 (frp/filter odd? e1)
        e3 (frp/reduce + 0 e2)
        e4 (frp/defer 2000 e3)
        ]
    ; (frp/consume! e0 #(println [:e0 %1]))
    ; (frp/consume! e1 #(println [:e1 %1]))
    ; (frp/consume! e2 #(println [:e2 %1]))
    ; (frp/consume! e3 #(println [:e3 %1]))
    ; (frp/consume! e4 #(println [:e4 %1]))
    ; (println (frp/join frp/never frp/never e1 e2))
    ; (frp/push! e0 0)
    ; (frp/push! e0 1)
    ; (frp/push! e0 2)
    ; (frp/push! e0 3)
    ; (frp/push! e0 4)
    ; (frp/push! e0 5)
    ; (frp/push! e0 6)
    ; (frp/push! e0 7)
    ; (frp/push! e0 8)
    ; (frp/push! e0 9)
    (println js/snabbdom.init)
    ))
;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/items"
     ["" :items]
     ["/:item-id" :item]]
    ["/about" :about]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(defn home-page []
  (fn []
    (dom/run {} ui #(pprint %1))
    [:span.main
     [:h1 "Welcome to mayu"]
     [:button {:on-click run-frp} "Test Me!"]
     [:ul
      [:li [:a {:href (path-for :items)} "Items of mayu"]]
      [:li [:a {:href "/broken/link"} "Broken link"]]]]))



(defn items-page []
  (fn []
    [:span.main
     [:h1 "The items of mayu"]
     [:ul (map (fn [item-id]
                 [:li {:name (str "item-" item-id) :key (str "item-" item-id)}
                  [:a {:href (path-for :item {:item-id item-id})} "Item: " item-id]])
               (range 1 60))]]))


(defn item-page []
  (fn []
    (let [routing-data (session/get :route)
          item (get-in routing-data [:route-params :item-id])]
      [:span.main
       [:h1 (str "Item " item " of mayu")]
       [:p [:a {:href (path-for :items)} "Back to the list of items"]]])))


(defn about-page []
  (fn [] [:span.main
          [:h1 "About mayu"]]))


;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :items #'items-page
    :item #'item-page))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About mayu"]]]
       [page]
       [:footer
        [:p "mayu was generated by the "
         [:a {:href "https://github.com/reagent-project/reagent-template"} "Reagent Template"] "."]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (rdom/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)
        ))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
