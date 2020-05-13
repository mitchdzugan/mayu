(ns mayu.dom.to-string.attrs
  (:require [clojure.string :as str]))

;; ported to from hiccup
;; https://github.com/weavejester/hiccup

(defprotocol AsString
  (to-str [x] "Convert a value into a string."))

(extend-protocol AsString
  #?(:clj java.lang.String :cljs string)
  (to-str [s] s)

  #?(:clj clojure.lang.Keyword :cljs cljs.core.Keyword)
  (to-str [k] (name k))
  nil
  (to-str [_] ""))

(defn as-str
  "Converts its arguments into a string using [[to-str]]."
  [& xs]
  (apply str (map to-str xs)))

(defn- escape-html
  "Change special characters into HTML character entities."
  [text]
  (.. (as-str text)
      (replace "&"  "&amp;")
      (replace "<"  "&lt;")
      (replace ">"  "&gt;")
      (replace "\"" "&quot;")
      (replace "'" "&apos;")))

(defn- render-style-map [value]
  (->> value
       (map (fn [[k v]] (str (as-str k) ":" v ";")))
       (sort)
       (apply str)))

(defn- render-attr-value [value]
  (cond
    (map? value) (render-style-map value)
    (sequential? value) (str/join " " value)
    :else value))

(defn- xml-attribute [name value]
  (str " " (as-str name) "=\"" (escape-html (render-attr-value value)) "\""))

(defn- render-attribute [[name value]]
  (cond
    (true? value) (str " " (as-str name))
    (not value) ""
    :else (xml-attribute name value)))

(defn render-attr-map
  "Render a map of attributes."
  [attrs]
  (apply str (sort (map render-attribute attrs))))

(defn render-class [c]
  (let [to-single #(reduce str "" (interpose " " %))]
    (cond (map? c) (->> (keys c)
                        (filter (partial get c))
                        (map name)
                        to-single)
          (coll? c) (to-single c)
          :else c)))
