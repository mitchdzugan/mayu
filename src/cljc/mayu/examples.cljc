(ns mayu.examples
  (:require [allpa.core :as a
             :refer [varg# defprotomethod]]
            [mayu.async
             :refer [go go-loop timeout <! chan >! close!]]
            [mayu.macros
             :refer [defui ui defui-cached]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.c-one :as c-one]
            [mayu.c-two :as c-two]
            [mayu.dom :as dom]
            [mayu.dom.to-string.attrs :as attrs]))

(defui my-component [n]
  (= 0 n) --> <[span "loading"]
  <[ul $=
    <[dom/stash $=
      <[li "was"]
      <[li "out"]
      <[keyed "Test"
        <[li "of"]]
      <[li "order"]
      ] stashed >
    <[li {:class "aclass"} "You!"]
    <[li "thought"]
    <[li "this"]
    <[dom/unstash stashed]]
  <[h3 "this is a header"]
  <[p "this is a test paragraph!!"])

(defui change-score-button [change]
  <[button (str "Click for " change " points")] btn >
  (dom/emit ::score (e/map (varg# change) (dom/on-click btn)))
  [(dom/on-click btn)])

(defui score-display [score]
  <[div (str "Score: " score)])

(defui scores []
  <[dom/collect-and-reduce ::score #(+ %1 %2) 0 $=
    [(println "Rendering Component Top Level")]
    s-score <- (dom/envs ::score)
    <[dom/bind s-score $[score]=
      <[dom/collect-and-reduce ::score2 #(+ %1 %2) 0 $=
        <[button "Inc scores"] btn >
        (dom/emit ::score2 (e/map (varg# 1) (dom/on-click btn)))
        s-score2 <- (dom/envs ::score2)
        <[dom/bind s-score2 $[score2]=
          <[div (str "Score 2: " score2)]]]
      <[change-score-button 1]>
      <[score-display score]
      <[change-score-button -1]>]])

(defui error-alert []
  <[div {:class "uk-alert-danger"} $=
    <[p "An unexpected error occurred"]])

(defui progress-bar [value max]
  <[progress {:class "uk-progress" :value value :max max}])

(defui countdown []
  s-timer <- (s/reduce inc 0 (e/timer 300))
  <[dom/collect-and-reduce ::error #(-> %2) nil $=
    <[div {:class "uk-card uk-card-default uk-card-body"} $=
      <[h3 {:class "uk-card-title"} "Content"]
      <[div {:class "uk-card-body"} $=
        s-error <- (dom/envs ::error)
        <[dom/bind s-error $[error]=
          <[dom/bind s-timer $[timer]=
            (not (nil? error)) --> <[error-alert]
            (<= timer 10)      --> <[progress-bar timer 10]
            <[table {:class "uk-table"} $=
              <[thead $=
                <[tr $=
                  <[th "Column 1"]
                  <[th "Column 2"]
                  <[th "Column 3"]]]
              <[tbody $=
                <[tr $=
                  <[td "Data 1.a"]
                  <[td "Data 2.a"]
                  <[td "Data 3.a"]]
                <[tr $=
                  <[td "Data 1.b"]
                  <[td "Data 2.b"]
                  <[td "Data 3.b"]]]]]]]
      <[div {:class "uk-card-footer"} $=
        <[a {:href "#" :class "uk-button uk-button-text"} "Set Error"] e-btn >
        (dom/emit ::error (e/map (varg# true) (dom/on-click e-btn)))]]])

(defui alert-system [e-alert]
  let [e-alert-off (e/map (varg# nil) (e/defer e-alert 2000))]
  s-alert <- (s/from nil (e/join e-alert e-alert-off))
  <[dom/bind s-alert $[alert]=
    <[when (not (nil? alert))
      <[div {:class "fixed"} $=
        <[div {:class "uk-card uk-card-default uk-card-body"} $=
          <[dom/unstash alert]]]]])

(defui add-alert-btn [btn-label]
  context-text <- (dom/envs ::context-text)
  <[dom/stash $= <[div context-text]] alert-stash >
  <[button btn-label] alert-btn >
  (dom/emit ::alert (e/map (varg# alert-stash) (dom/on-click alert-btn))))

(defui stash-demo3 []
  <[dom/collect ::alert $[e-alert]=
    <[dom/assoc-env ::context-text "From First Button" $=
      <[add-alert-btn "button 1"]]
    <[p "Some other text"]
    <[dom/assoc-env ::context-text "From Another Place" $=
      <[div $=
        <[p "Random Text..."]
        <[add-alert-btn "another button"]]]
    <[alert-system e-alert]])

(defui stash-demo2 []
  <[dom/assoc-env ::message "Bad Text" $=
    <[dom/assoc-env ::message "Good Text" $=
      <[dom/stash $=
        message <- (dom/envs ::message)
        <[p message]
        ] stashed >
      <[dom/unstash stashed]
      [stashed]
      ] stashed >
    <[dom/unstash stashed]])

(defui stash-demo1 []
  <[ul $=
    <[dom/stash $=
      <[li_ "appear"]
      <[li "in"]
      <[li "order"]
      ] stashed >
    <[li "These"]
    <[li "messages"]
    <[li "will"]
    <[dom/unstash stashed]])

(defui offs-test []
  <[dom/collect-and-reduce ::score #(+ %1 %2) 0 $=
    [(println "Rendering Component Top Level")]
    s-score <- (dom/envs ::score)
    <[dom/bind s-score $[score]=
      <[keyed score
        <[dom/collect-and-reduce ::score2 #(-> %2) 0 $=
          s-score2 <- (dom/envs ::score2)
          <[button "Inc scores"] btn >
          <[dom/bind s-score2 $[score2]=
            s-timer <- (s/reduce inc 0 (e/timer 300))
            (dom/emit ::score2 (e/tag-with +
                                           (e/map (varg# 1000)
                                                  (dom/on-click btn))
                                           s-timer))
            <[dom/bind s-timer $[timer]=
              <[dom/memo true $=
                [(println "rendering memo")]
                <[p "hello"]]
              <[p (str timer)]]
            <[div (str "Score2: " score2)]]]]
      <[change-score-button 1]>
      <[score-display score]
      <[change-score-button -1]>]])

(defui memo-test []
  <[dom/collect-and-reduce ::score #(+ %1 %2) 0 $=
    <[dom/collect-and-reduce ::score3 inc 0 $=
      s-score3 <- (dom/envs ::score3)
      <[button "Inc scores 3"] btn >
      (dom/emit ::score3 (dom/on-click btn))
      [(println "Rendering Component Top Level")]
      s-score <- (dom/envs ::score)
      <[dom/bind s-score $[score]=
        <[keyed (quot score 4)
          <[dom/bind s-score3 $[score3]=
            <[div (str "Score3: " score3)]
            <[dom/memo true $=
              <[dom/collect-and-reduce ::score2 inc 0 $=
                s-score2 <- (dom/envs ::score2)
                <[button "Inc scores"] btn >
                (dom/emit ::score2 (dom/on-click btn))
                <[dom/bind s-score2 $[score2]=
                  <[div (str "Score2: " score2)]]]]]]
        <[change-score-button 1]>
        <[score-display score]
        <[change-score-button -1]>]]])

(defui switcher [n child-map]
  <[case (odd? n)
    <[true (:odd child-map)]
    <[false (:even child-map)]])

(defui switcher-f [n child-map]
  <[case (odd? n)
    <[true ((:odd child-map) "lmao")]
    <[false ((:even child-map) "lmao")]])

(defui special-syms []
  ; [(println (macroexpand-1) '(ui))]
  let [!ui
       test-ui (fn [a] <[p a])

       !ui
       !rec
       rec-1 (fn [n] <[case n
                       <[0 <[div "1.0"]]
                       <[:else <[div $=
                                 <[dom/text (str "1." n)]
                                 <[rec-1 (dec n)]]]])

       !rec
       !ui
       rec-2 (fn [n] <[case n
                       <[0 <[div "2.0"]]
                       <[:else <[div $=
                                 <[dom/text (str "2." n)]
                                 <[rec-2 (dec n)]]]])]
  <[rec-1 4]
  <[rec-2 4]
  <[test-ui "ayy lmao"]
  <[test-ui "xD"]
  <[multi switcher 1 $=
    <[:odd <[p "odd"]]
    <[:even <[p "even"]]]
  <[multi switcher 2 $=
    <[:odd <[p "odd"]]
    <[:even <[p "even"]]]
  <[multi switcher 3 $=
    <[:odd <[p "odd"]]
    <[:even <[p "even"]]]
  <[multi-f switcher-f 1 $=
    <[:odd $[s]= <[p (str "odd " s)]]
    <[:even $[s]= <[p (str "even " s)]]]
  <[multi-f switcher-f 2 $=
    <[:odd $[s]= <[p (str "odd " s)]]
    <[:even $[s]= <[p (str "even " s)]]]
  <[multi-f switcher-f 3 $=
    <[:odd $[s]= <[p (str "odd " s)]]
    <[:even $[s]= <[p (str "even " s)]]]
  <[ul $=
    <[for ["this" "is" "a" "list"] $[s]=
      <[li s]]]
  <[case 3
    <[4 <[p "not this"]]
    <[3 <[p "This!"]]
    <[2 <[p "noep"]]]
  <[case 3
    <[4 <[p "not this"]]
    <[2 <[p "noep"]]
    <[:else <[p {:class {:a true :b false}} "none of the above"]]]
  <[condp = 3
    <[4 <[p "not this"]]
    <[3 <[p "This!"]]
    <[2 <[p "noep"]]]
  <[condp = 3
    <[4 <[p "not this"]]
    <[2 <[p "noep"]]
    <[:else <[p "none of the above"]]]
  <[cond
    <[(= 0 1) <[p "not this"]]
    <[(= 1 1)
      <[p "this"]
      <[p "with multiple lines"]]]
  <[cond
    <[(= 0 1) <[p "not this"]]
    <[:else
      <[p "this"]
      <[p "with multiple lines"]]]
  <[when true
    <[p "when true"]
    <[p "as much as you want"]]
  <[when false <[p "cant see me"]]
  <[when-not false <[p "when-not false"]]
  <[when-not true <[p "cant see me"]]
  <[if (= 1 1)
    <[then <[p "Yay!"]]
    <[else <[p "boo..."]]]
  <[if (= 0 1)
    <[then <[p "umm"]]
    <[else <[p "Nice!"]]])

(defn slow-fib [n]
  (if (< n 2)
    1
    (+ (slow-fib (dec n))
       (slow-fib (dec (dec n))))))

;; this component draws a styled input component and label and
;; returns an event that fires everytime its input changes with
;; the value it is being changed to
(def cc (atom 1))
(defui styled-input [k label]
  ;; for every dom tag like `div` there is a corresponding
  ;; version ending in _ like `div_` that returns the result
  ;; of its children instead of itself.
  <[div_ {:class "uk-card uk-card-default uk-card-body"} $=
    <[label_ $=
      <[dom/text label]
      sig <- (dom/envs k)
      ;; `apply` runs its children and then returns the result of
      ;; applying its 1st function argument to that result. In this
      ;; case we apply `s/unwrap-event` which converts a signal of
      ;; events to an event. This is because `dom/bind` returns a
      ;; signal which contains the result of its children over time
      ;; as its input signal changes.
      <[apply s/unwrap-event
        <[dom/bind sig $[val]=
          <[button "Btn"] el-btn >
          (dom/consume! (dom/on-click el-btn) #(do (println (str "Clicked! " @cc))
                                                   (swap! cc inc)))
          <[input {:value val}] el-input >
          [(->> (dom/on-input el-input)
                (e/map #(.. % -target -value))
                (#(e/dedup % val)))]]]]])

(defui inputs-demo []
  <[dom/collect-values ::upper "" $=
    e-changed <- <[styled-input ::upper "Force uppercase"]
    (dom/emit ::upper (e/map #(do (println (slow-fib 40))
                                  (clojure.string/upper-case %))
                             e-changed))]
  <[dom/collect-values ::max-5 "" $=
    e-changed <- <[styled-input ::max-5 "Max 5 chars"]
    (dom/emit ::max-5 (e/filter #(<= (count %1) 5) e-changed))])

(defui syntax-demo []
  <[div {:class "uk-card uk-card-default uk-card-body"} $=
    <[p "some message"]]
  #_[ul $=
    <[li "1"]
    <[li "2"]
    <[li "3"]
    <[li "4"]
     <[li "5"]])

(defrecord Append [])

(defrecord Remove [id])

(defprotomethod reduce-action [{:keys [id]} items]
  Append
  (conj items {:id (->> items (apply max-key :id) :id inc)})
  Remove
  (->> items (remove #(= id (:id %1))) vec))

(defui animations-demo []
  <[dom/collect-reduce-and-bind ::items (a/flip reduce-action) [] $[items]=
    <[dom/collect-reduce-and-bind ::counter inc 0 $[counter]=
      <[button "INC"] btn1 >
      <[p (str "counter " counter)]
      (dom/emit ::counter (dom/on-click btn1))
      <[button {:style {:margin-left "55px"}} "+"] btn >
      (dom/emit ::items (e/map (varg# (->Append)) (dom/on-click btn)))
      <[keyed counter
        <[div {:class "no-transition"
               :delayed {:class ["test" "123"]}}
          $=
          <[for items $[{:keys [id]}]=
            <[keyed id
              <[div {:class "transition"
                     :style {:display "flex"
                             :align-items "stretch"
                             :margin "5px"
                             :transform "scale(0, 0)"
                             :delayed {:transform "scale(1, 1)"}
                             :remove {:transform "scale(0, 0)"}}} $=
                <[div {:style {:background-color "#74f7df"
                               :width "50px"}}]
                <[button "x"] btn >
                (dom/emit ::items (e/map (varg# (->Remove id)) (dom/on-click btn)))]]]]]]])

(defui ssr-await-demo []
  s-timer <- (s/reduce inc 0 (e/timer 1000))
  <[dom/bind s-timer $[timer]=
    <[ul {:class ["a" "b"]
          :delayed {:class ["c" "d"]}} $=
      <[li {:style {:display "none"
                    :delayed {:display "block"}
                    :remove {:display "none"}}
            :class {:a true :b false :c true}} "Begin"]
      <[ssr-await (>= timer 2) 4000
        <[timeout <[li "Failed to load in time"]]
        <[then    <[li "Slow loading 1"]]]
      <[ssr-await (>= timer 8) 4000
        <[timeout <[li "Failed to load in time"]]
        <[then    <[li "Slow loading 2"]]]
      <[ssr-await (>= timer 2) 4000
        <[timeout <[li "Failed to load in time"]]
        <[then    <[li "Slow loading 3"]]]
      <[li {:class ["a" "b"]} "End"]]])

(defui min-repro []
  <[dom/collect-and-reduce ::counter inc 0 $=
    <[button "Click"] btn >
    (dom/emit ::counter (dom/on-click btn))
    s-counter <- (dom/envs ::counter)
    s-c1 <- (s/map inc s-counter)
    s-c2 <- (s/map dec s-counter)
    <[dom/bind s-counter $[counter]= <[p (str counter)]]
    <[dom/bind s-c1 $[c1]= <[p (str c1)]]
    <[dom/bind s-c2 $[c2]= <[p (str c2)]]])

(defui min-repro-2 []
  <[dom/collect-and-reduce ::counter inc 0 $=
    <[button "Click"] btn >
    (dom/emit ::counter (dom/on-click btn))
    s-counter <- (dom/envs ::counter)
    s-mod <- (s/map #(mod % 2) s-counter)
    <[dom/bind s-mod $[mod]=
      <[case mod
        <[0
          <[p "Even Page"]
          s-c2 <- (s/map dec s-counter)
          <[dom/bind s-c2 $[c2]= <[p (str "c2:" c2)]]]
        <[1
          <[p "Odd Page"]
          s-c2 <- (s/map inc s-counter)
          <[dom/bind s-c2 $[c2]= <[p (str "c2:" c2)]]]]]])

(defn log [& args]
  #?(:clj (println args) :cljs (apply js/console.log (clj->js args))))

(defn logger [k]
  (fn [a]
    (#?(:clj println :cljs js/console.log) k a)))

(defui min-repro-3 []
  <[div {:class "a"} $=
    e-mount <- dom/get-mount-event
    (dom/consume! e-mount (logger :a))
    <[dom/collect-reduce-and-bind :c1 inc 0 $[c1]=
      <[div {:class "b"} $=
        e-mount <- dom/get-mount-event
        (dom/consume! e-mount (logger :b))]
      <[keyed (quot c1 3)
        <[div {:class "c"} $=
          e-mount <- dom/get-mount-event
          (dom/consume! e-mount (logger :c))]
        <[dom/collect-reduce-and-bind :c2 inc 0 $[c2]=
          <[p $= <[span (str "c1 " c1)]]
          <[p $= <[span (str "c2 " c2)]]
          <[button "b1"] b1 >
          <[button "b2"] b2 >
          (dom/emit :c1 (dom/on-click b1))
          (dom/emit :c2 (dom/on-click b2))
          s-c1 <- (dom/envs :c1)
          <[dom/bind s-c1 $[c1]=
            <[dom/collect-reduce-and-bind :c3 inc 0 $[c3]=
              <[button "b2.2"] b3 >
              (dom/emit :c1 (dom/on-click b3))
              <[p $= <[span (str "c3 " c3)]]]]]]]])

(defui stop-prop []
  <[dom/collect-reduce-and-bind :a inc 0 $[a]=
    <[div $=
      <[dom/collect-reduce-and-bind :b inc 0 $[b]=
        <[p (str "A: " a)]
        <[p (str "B: " b)]
        <[button "B"] d-b >
        (->> (dom/on-click d-b) dom/stop-propagation (dom/emit :b))]
      ] d-a >
    (dom/emit :a (dom/on-click {:capture false} d-a))])

(defui disabled-example []
  <[dom/collect-reduce-and-bind :a inc 0 $[a]=
    <[dom/collect-reduce-and-bind :b inc 0 $[b]=
      <[p a]
      <[a "Click A"] btn-a >
      <[p b]
      <[a {:disabled (odd? a)} "Click B"] btn-b >
      (->> (dom/on-click btn-b)
           dom/remove-disabled
           (dom/emit :b))
      (->> (dom/on-click btn-a)
           (dom/emit :a))]])

(defui min-repro-4 []
  <[dom/collect-reduce-and-bind :a inc 0 $[a]=
    <[p a]
    <[button "b1"] b1 >
    <[button "b2"] b2 >
    (->> (dom/on-click b1)
         (e/map #(do (println :g) %))
         (dom/emit :a))
    (dom/consume! (->> (dom/on-click b2)
                       (e/map identity)
                       (e/map #(do (println :f) %)))
                  println)])

(defn on-render [el]
  (aset el "scrollTop" 50))

(defui on-render-test []
  <[dom/collect-reduce-and-bind :a inc 0 $[a]=
    <[when (> a 3)
      <[div {:on-render on-render
             :style {:width "200px"
                     :height "200px"
                     :border "1px solid black"
                     :overflow "scroll"}} $=
        <[ul $=
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]
          <[li "1"]]]]
    <[p a]
    <[button "Inc"] btn >
    (->> (dom/on-click btn)
         (dom/emit :a))])

(defui min-repro-5 []
  <[dom/collect-reduce-and-bind :c inc 0 $[c]=
    let [is-odd? (odd? c)]
    ((if is-odd? c-one/c c-two/c))
    <[button "Click"] btn >
    <[p is-odd?]
    (dom/emit :c (dom/on-click btn))])

(defn sqrt [v]
  #?(:clj (Math/sqrt v)
     :cljs (js/Math.sqrt v)))

(defui-cached square-rootify :cache [t s]
  (s/map #(do (log :MAPPING_SQR_RT t %1)
              {:raw %1 :val (sqrt %1)}) s))

(defui test-row [timer1 active? s-timer-2]
  <[div {:class {:active active?}} $=
    <[div timer1]
    <[div (str (if active? "A" "Ina") "ctive")]
    <[if active?
      <[then
        s-sqrt <- <[square-rootify :CURR s-timer-2]
        <[dom/bind s-sqrt $[{:keys [raw val]}]=
          <[div (str raw " : " (.toFixed val 2))]]]
      <[else <[div]]]])

(defui test-cache-curr []
  s-timer-1 <- (s/reduce inc 0 (e/timer 11000))
  s-timer-2 <- (s/reduce inc 0 (e/timer 1389))
  <[dom/provide-cache :cache $=
    <[div {:class "container"} $=
      <[dom/bind s-timer-1 $[timer1]=
        let [row-1-active? (> 2 (mod timer1 7))
             row-2-active? (< 3 (mod timer1 6))
             row-3-active? (> 3 (mod (+ 1 timer1) 5))]
        <[test-row timer1 row-1-active? s-timer-2]
        <[test-row timer1 row-2-active? s-timer-2]
        <[test-row timer1 row-3-active? s-timer-2]]]])

(defui square-rootify- [t s]
  (s/map #(do (log :MAPPING_SQR_RT t %1)
              {:raw %1 :val (sqrt %1)}) s))

(defui test-row-old1 [timer1 active? s-timer-2]
  <[div {:class {:active active?}} $=
    <[div timer1]
    <[div (str (if active? "A" "Ina") "ctive")]
    <[if active?
      <[then
        s-sqrt <- (square-rootify- :OLD1 s-timer-2)
        <[dom/bind s-sqrt $[{:keys [raw val]}]=
          <[div (str raw " : " (.toFixed val 2))]]]
      <[else <[div]]]])

(defui test-cache-old1 []
  s-timer-1 <- (s/reduce inc 0 (e/timer 11000))
  s-timer-2 <- (s/reduce inc 0 (e/timer 1389))
  <[div {:class "container"} $=
    <[dom/bind s-timer-1 $[timer1]=
      let [row-1-active? (> 2 (mod timer1 7))
           row-2-active? (< 3 (mod timer1 6))
           row-3-active? (> 3 (mod (+ 1 timer1) 5))]
      <[test-row-old1 timer1 row-1-active? s-timer-2]
      <[test-row-old1 timer1 row-2-active? s-timer-2]
      <[test-row-old1 timer1 row-3-active? s-timer-2]]])

(defui test-row-old2 [timer1 active? s-timer-2]
  <[div {:class {:active active?}} $=
    <[div timer1]
    <[div (str (if active? "A" "Ina") "ctive")]
    <[if active?
      <[then
        s-sqrt <- (dom/envs :s-sqrt)
        <[dom/bind s-sqrt $[{:keys [raw val]}]=
          <[div (str raw " : " (.toFixed val 2))]]]
      <[else <[div]]]])

(defui test-cache-old2 []
  s-timer-1 <- (s/reduce inc 0 (e/timer 11000))
  s-timer-2 <- (s/reduce inc 0 (e/timer 1389))
  s-sqrt <- (square-rootify- :OLD2 s-timer-2)
  <[dom/assoc-env :s-sqrt s-sqrt $=
    <[div {:class "container"} $=
      <[dom/bind s-timer-1 $[timer1]=
        let [row-1-active? (> 2 (mod timer1 7))
             row-2-active? (< 3 (mod timer1 6))
             row-3-active? (> 3 (mod (+ 1 timer1) 5))]
        <[test-row-old2 timer1 row-1-active? s-timer-2]
        <[test-row-old2 timer1 row-2-active? s-timer-2]
        <[test-row-old2 timer1 row-3-active? s-timer-2]]]])

(defui test-caches []
  <[style (str ".container {"
               "  display: flex;"
               "  flex-direction: column;"
               "}"
               ".container > div {"
               "  margin: 5px;"
               "  width: 400px;"
               "  display: flex;"
               "  flex-direction: row;"
               "  justify-content: center;"
               "  background: #cbcbcb;"
               "  border-radius: 5px;"
               "  border: 1px solid black;"
               "}"
               ".container > div.active {"
               "  background: #ffbbbb;"
               "}"
               ".container > div > div:first-child, .container > div > div:last-child {"
               "  font-size: 12px;"
               "  flex: 0 0 100px;"
               "}"
               ".container > div > div:nth-child(2) {"
               "  flex: 1;"
               "}"
               ".container > div > div {"
               "  text-align: center;"
               "}"
               )]
  ; const urlParams = new URLSearchParams(window.location.search);
  ; const myParam = urlParams.get('myParam');

  let [url-params #?(:clj nil
                     :cljs (js/URLSearchParams. (.. js/window -location -search)))
       my-param (.get url-params "impl")]

  <[case my-param
    <["old1" <[test-cache-old1]]
    <["old2" <[test-cache-old2]]
    <[:else <[test-cache-curr]]])

(defui my-ui []
  <[div]
  <[div $=
    [(let [c (dom/render-to-string {} ssr-await-demo)]
       (go-loop []
         (let [more (<! c)]
           (when more
             (println more)
             (recur)))))]
    <[min-repro-5]
    <[on-render-test]
    <[min-repro-4]
    <[disabled-example]
    <[stop-prop]
    <[div "1"]
    <[div 1]
    <[div nil]
    <[div $= <[p "1"]]
    <[min-repro-2]
    <[min-repro-3]
    <[ssr-await-demo]
    <[min-repro]
    <[animations-demo]
    <[syntax-demo]
    <[inputs-demo]
    <[offs-test]
    <[memo-test]
    <[stash-demo1]
    <[stash-demo2]
    <[stash-demo3]
    <[countdown]
    <[scores]
    <[test-caches]
    <[special-syms]])


