(ns core
  (:require  [clojure.test :refer :all]
             [allpa.core
              :refer [curry]]
             [mayu.frp.event :as e]))

(deftest events
  (testing "push/consume/off"
    (let [consumed (atom [])
          e (e/on! (e/Event))]
      (e/push! e 1)
      (e/push! e 2)
      (let [off (e/consume! e #(swap! consumed (curry conj %1)))]
        (e/push! e 3)
        (e/push! e 4)
        (off)
        (e/push! e 5)
        (e/push! e 6)
        (is (= @consumed [3 4])))))
  (testing "map"
    (let [consumed (atom [])
          e (e/on! (e/Event))
          mapped (e/map inc e)]
      (e/push! e 1)
      (e/push! e 2)
      (let [off (e/consume! mapped #(swap! consumed (curry conj %1)))]
        (e/push! e 3)
        (e/push! e 4)
        (off)
        (e/push! e 5)
        (e/push! e 6)
        (is (= @consumed [4 5])))))
  (testing "filter"
    (let [consumed (atom [])
          e (e/on! (e/Event))
          filtered (e/filter odd? e)]
      (e/push! e 1)
      (e/push! e 2)
      (let [off (e/consume! filtered #(swap! consumed (curry conj %1)))]
        (e/push! e 3)
        (e/push! e 4)
        (off)
        (e/push! e 5)
        (e/push! e 6)
        (is (= @consumed [3])))))
  (testing "reduce"
    (let [consumed (atom [])
          e (e/on! (e/Event))
          reduced (e/reduce + 0 e)
          off (e/consume! reduced #(swap! consumed (curry conj %1)))]
      (e/push! e 1)
      (e/push! e 2)
      (e/push! e 3)
      (e/push! e 4)
      (off)
      (e/push! e 5)
      (e/push! e 6)
      (is (= @consumed [1 3 6 10]))))
  (testing "flat-map"
    (let [e1 (e/on! (e/Event))
          e2 (e/on! (e/Event))
          e3 (e/on! (e/Event))
          e4 (e/flat-map #(if (odd? %1) e3 e2) e1)
          consumed (atom [])
          off (e/consume! e4 #(swap! consumed (curry conj %1)))]
      (e/push! e1 1)
      (e/push! e3 [:e3 1])
      (e/push! e3 [:e3 2])
      (e/push! e3 [:e3 3])
      (e/push! e2 [:e2 1])
      (e/push! e2 [:e2 2])
      (e/push! e2 [:e2 3])
      (e/push! e1 2)
      (e/push! e3 [:e3 4])
      (e/push! e3 [:e3 5])
      (e/push! e2 [:e2 4])
      (e/push! e2 [:e2 5])
      (off)
      (is (= @consumed [[:e3 1]
                        [:e3 2]
                        [:e3 3]
                        [:e2 4]
                        [:e2 5]]))))
  (testing "join"
    (let [e1 (e/on! (e/Event))
          e2 (->> e1
                  (e/filter #(< %1 6))
                  (e/map #(-> [%1 :e2])))
          e3 (->> e1
                  (e/filter #(< %1 5))
                  (e/map #(-> [%1 :e3])))
          e4 (->> e1
                  (e/filter #(< %1 3))
                  (e/map #(-> [%1 :e4])))
          e5 (e/join-skip-siblings e2 e3 e4)
          consumed (atom [])
          off (e/consume! e5 #(swap! consumed (curry conj %1)))]
      (e/push! e1 1)
      (e/push! e1 2)
      (e/push! e1 3)
      (e/push! e1 4)
      (e/push! e1 5)
      (e/push! e1 6)
      (off)
      (is (= @consumed [[1 :e4]
                        [2 :e4]
                        [3 :e3]
                        [4 :e3]
                        [5 :e2]])))
    (let [e1 (e/on! (e/Event))
          e2 (->> e1
                  (e/filter #(< %1 6))
                  (e/map #(-> [%1 :e2])))
          e3 (->> e1
                  (e/filter #(< %1 5))
                  (e/map #(-> [%1 :e3])))
          e4 (->> e1
                  (e/filter #(< %1 3))
                  (e/map #(-> [%1 :e4])))
          e5 (e/join e2 e3 e4)
          consumed (atom [])
          off (e/consume! e5 #(swap! consumed (curry conj %1)))]
      (e/push! e1 1)
      (e/push! e1 2)
      (e/push! e1 3)
      (e/push! e1 4)
      (e/push! e1 5)
      (e/push! e1 6)
      (off)
      (is (= @consumed [[1 :e3]
                        [1 :e2]
                        [1 :e4]
                        [2 :e3]
                        [2 :e2]
                        [2 :e4]
                        [3 :e3]
                        [3 :e2]
                        [4 :e3]
                        [4 :e2]
                        [5 :e2]])))))

