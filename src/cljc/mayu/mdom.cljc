(ns mayu.mdom)

(defrecord Text [text])
(defrecord CreateElement [tag attrs children])
(defrecord Bind [a-bind])

(defui stuff
  <[div {:className "wrapper"} $=
    <[button "Click Me"]d_clickSource>
    s_count <- (s_from (-> (:onClick d_clickSource)
                           (frp/reduce #(inc %1) 0))
                       0)
    <[bindDOM s_count $[count]=
      <[span (str "Clicked " count " times")]>
      ]>]>)


