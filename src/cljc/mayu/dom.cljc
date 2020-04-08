(ns mayu.dom
  (:require [allpa.core
             :refer [curry]]
            [mayu.mdom
             :refer [MText MCreateElement MBind]]
            [wayra.core :as w
             :refer [defnm defm mdo]]))

(defm env (w/asks :env))

(defnm envs [f]
  e <- env
  [(f e)])

(defnm assoc-env [k v m] (w/local #(assoc-in %1 [:env k] v) m))

(defnm assoc-in-env [ks v m] (w/local #(assoc-in %1 (concat [:env] ks) v) m))

(defnm update-env [k f m] (w/local #(update-in %1 [:env k] f) m))

(defnm update-in-env [ks f m] (w/local #(update-in %1 (concat [:env] ks) f) m))

(defnm text [s] (w/tell {:mdom (MText s)}))

(defnm create-element
  ([tag] (create-element tag {} (w/pure nil)))
  ([tag arg]
   (let [[attrs m] (if (map? arg)
                     [arg (w/pure nil)]
                     [{} arg])]
     (create-element tag attrs m)))
  ([tag attrs m-]
   (let [m (if (string? m-) (text m-) m-)]
     (w/pass (mdo res <- m
                  [[res
                    (curry update :mdom
                           #(-> [(MCreateElement tag attrs %1)]))]])))))

(defnm emit [at e] (w/tell {:events {at e}}))

;; TODO need preempt implemented
(defnm collect [])

(defnm stash-dom [m]
  (w/pass (mdo [res, w] <- (w/listen m)
               [[{:res res :mdom (:mdom w)}
                 (curry assoc :mdom [])]])))

(defnm apply-stash [{:keys [mdom]}] (w/eachm mdom #(w/tell {:mdom %1})))

(defn run [env m use-mdom]
  (->> m
       (w/exec {:reader {:env env}
                :init-writer {:mdom []}})
       :writer
       :mdom
       use-mdom))
