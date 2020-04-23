(ns mayu.macros
  (:require [wayra.core :as w
             :refer [mdo defnm fnm <#>]]
            [mayu.tags :as tags]
            [mayu.dom :as dom])
  #?(:cljs (:require-macros [mayu.macros :refer [ui defui]])))

(comment :special-cases
         <[if bool
           <[then ...]
           <[else ...]]
         <[case x
           <[1 <[]]
           ]
         )

#?(:clj
   (defmacro ui [& body]
     (let [mk-args
           (fn [els]
             (let [[f & args] els]
               (condp = f
                 ;; TODO ensure valid syntax and error message if not
                 'if `(if ~(nth args 0)
                        (dom/step ::if-then (ui ~@(drop 1 (nth args 2))))
                        (dom/step ::if-else (ui ~@(drop 1 (nth args 4)))))
                 'when `(if ~(nth args 0)
                          (dom/step ::when (ui ~@(drop 1 args)))
                          (w/pure nil))
                 'when-not `(if ~(nth args 0)
                              (w/pure nil)
                              (dom/step ::when-not (ui ~@(drop 1 args))))
                 'case `(case ~(nth args 0)
                          ~@(mapcat (fn [clause id]
                                      (let [matcher (nth clause 0)
                                            else? (= :else matcher)
                                            body
                                            `(dom/step ~(str "case." id)
                                                       (ui ~@(drop 1 clause)))]
                                        (if else? [body] [matcher body])))
                                    (filter vector? (drop 1 args))
                                    (range)))
                 'condp `(condp ~(nth args 0) ~(nth args 1)
                           ~@(mapcat (fn [clause id]
                                       (let [matcher (nth clause 0)
                                             else? (= :else matcher)
                                             body
                                             `(dom/step ~(str "condp." id)
                                                        (ui ~@(drop 1 clause)))]
                                         (if else? [body] [matcher body])))
                                     (filter vector? (drop 2 args))
                                     (range)))
                 'cond `(cond
                          ~@(mapcat (fn [clause id]
                                      (let [matcher (nth clause 0)
                                            body
                                            `(dom/step ~(str "cond." id)
                                                       (ui ~@(drop 1 clause)))]
                                        [matcher body]))
                                    (filter vector? args)
                                    (range)))
                 'for `(w/mapm (fn ~(nth args 2)
                                  (ui ~@(drop 4 args)))
                               ~(nth args 0))
                 'apply `(<#> (ui ~@(drop 1 args))
                                   ~(nth args 0))
                 (let [f (cond
                           (get tags/tag-map (name f))
                           `(partial mayu.dom/create-element ~(name f))

                           (get tags/tag_-map (name f))
                           `(partial mayu.dom/create-element_
                                     ~(subs (name f) 0 (dec (count (name f)))))

                           :else f)
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
                     :else els)))))]
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
