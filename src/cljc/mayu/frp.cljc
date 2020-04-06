(ns mayu.frp)

(defrecord Event [id on-required a-state])

(def a-nextId (atom 1))
(defn mk-event [on-required]
  (let [id @a-nextId]
    (swap! a-nextId inc)
    (Event. id on-required (atom {}))))
