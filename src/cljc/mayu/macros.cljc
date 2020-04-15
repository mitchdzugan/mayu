(ns mayu.macros
  (:require [wayra.core :as w
             :refer [mdo defnm fnm]]
            [mayu.tags :as tags]
            [mayu.dom :as dom])
  #?(:cljs (:require-macros [mayu.macros :refer [ui defui]])))

#?(:clj
   (defmacro ui [& body]
     (let [mk-args (fn [els]
                     (let [[f & args] els
                           f (if (get tags/tag-map (name f))
                               `(partial mayu.dom/create-element ~(name f))
                               f)
                           els (concat [f] args)
                           split (partition-by #(or (= '$ %1)
                                                    (= '= %1)
                                                    (= '$= %1))
                                               els)
                           scount (count split)]
                       (cond
                         (= 3 scount) (concat (nth split 0)
                                              [`(ui ~@(nth split 2))])
                         (= 5 scount) (concat (nth split 0)
                                              [`(fn ~(first (nth split 2))
                                                  (ui ~@(nth split 4)))])
                         :else els)))]
       `(mdo
          ~@(mapcat (fn [pthd psnd prev curr scnd thrd frth]
                      (cond
                        (and (= '< curr)
                             (vector? scnd)) []
                        (and (= '> curr)
                             (or (and (vector? prev)
                                      (= '< psnd))
                                 (and (vector? psnd)
                                      (= '< pthd)))) []
                        (and (vector? prev)
                             (= '> scnd)
                             (= '< psnd)) []
                        (and (vector? curr)
                             (= '< prev)
                             (= '> thrd)) [scnd '<- `(~@(mk-args curr))]
                        (and (vector? curr)
                             (= '< prev)) [`(~@(mk-args curr))]
                        (or (= '> curr)
                            (= '< curr))
                        (throw (Exception.
                                (str "\"<\" and \">\" are assumed to be the "
                                     "opening and closing of MDOM tags within "
                                     "Mayu's UI macros. If you need to you can "
                                     "use `mayu.util/lt` and `mayu.util/gt` "
                                     "instead.")))
                        :else [curr]))
                    (concat [nil nil nil] body)
                    (concat [nil nil] body)
                    (concat [nil] body)
                    body
                    (concat (drop 1 body) [nil])
                    (concat (drop 2 body) [nil nil])
                    (concat (drop 3 body) [nil nil nil]))))))

#?(:clj
   (defmacro defui [label args & body]
     `(defn ~label ~args (dom/step ~(str label) (ui ~@body)))))
