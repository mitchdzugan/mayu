(ns mayu.async
  #?(:clj (:require [net.cgrand.macrovich :as macros]
                    [clojure.core.async]
                    [clojure.walk
                     :refer [postwalk]])
     :cljs (:require [cljs.core.async]))
  #?(:cljs (:require-macros [cljs.core.async]
                            [mayu.async :refer [go go-loop alt!]])))

#?(:clj
   (defmacro go [& forms]
     `(~(macros/case :clj 'clojure.core.async/go
                     :cljs 'cljs.core.async/go)
       ~@(macros/case :clj (postwalk (fn [form]
                                       (cond
                                         (and (symbol? form)
                                              (= "<!" (name form)))
                                         'clojure.core.async/<!
                                         (and (symbol? form)
                                              (= ">!" (name form)))
                                         'clojure.core.async/>!
                                         (and (symbol? form)
                                              (= "alt!" (name form)))
                                         'clojure.core.async/alt!
                                         :else form))
                                     forms)
                      :cljs forms))))

#?(:clj
   (defmacro go-loop [& forms]
     `(~(macros/case :clj 'clojure.core.async/go-loop
                     :cljs 'cljs.core.async/go-loop)
       ~@(macros/case :clj (postwalk (fn [form]
                                       (cond
                                         (and (symbol? form)
                                              (= "<!" (name form)))
                                         'clojure.core.async/<!
                                         (and (symbol? form)
                                              (= ">!" (name form)))
                                         'clojure.core.async/>!
                                         (and (symbol? form)
                                              (= "alt!" (name form)))
                                         'clojure.core.async/alt!
                                         :else form))
                                     forms)
                      :cljs forms))))

#?(:clj
   (defmacro alt! [& forms]
     `(~(macros/case :clj 'clojure.core.async/alt!
                     :cljs 'cljs.core.async/alt!) ~@forms)))

(def chan #?(:clj clojure.core.async/chan
             :cljs cljs.core.async/chan))

(def put! #?(:clj clojure.core.async/put!
             :cljs cljs.core.async/put!))

(def take! #?(:clj clojure.core.async/take!
              :cljs cljs.core.async/take!))

(def >! #?(:clj clojure.core.async/>!
           :cljs cljs.core.async/>!))

(def <! #?(:clj clojure.core.async/<!
           :cljs cljs.core.async/<!))

(def buffer #?(:clj clojure.core.async/buffer
               :cljs cljs.core.async/buffer))

(def dropping-buffer #?(:clj clojure.core.async/dropping-buffer
                        :cljs cljs.core.async/dropping-buffer))

(def sliding-buffer #?(:clj clojure.core.async/sliding-buffer
                       :cljs cljs.core.async/sliding-buffer))

(def timeout #?(:clj clojure.core.async/timeout
                :cljs cljs.core.async/timeout))

(def close! #?(:clj clojure.core.async/close!
               :cljs cljs.core.async/close!))

(def alts! #?(:clj clojure.core.async/alts!
              :cljs cljs.core.async/alts!))

