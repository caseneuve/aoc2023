(ns day07.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [index-of]]))

(defn typ [h part]
  (let [f (frequencies h)
        J (if (= part 2) (get f \J 0) 0)
        [c1 c2 _ :as F] (sort > (vals (cond-> f (= 2 part) (dissoc \J))))]
    (cond
      (= 5     (+ J (or c1 0))) 7
      (= 4     (+ J c1))        6
      (= [3 2] [(+ J c1) c2])   5
      (= 3     (+ J c1))        4
      (= [2 2] [c1 c2])         3
      (= 2     (+ J c1))        2
      (= 5     (count F))       1
      :else                     0)))

(defn score [hand part]
  (let [cards (apply str (reverse (if (= part 2) "AKQT98765432J" "AKQJT98765432")))]
    (-> (map #(index-of cards %) hand) (conj (typ hand part)) vec)))

(defn winnings [input part]
  (->> input
       (map (fn [[hand bid]] [(score hand part) (parse-long bid)]))
       (sort-by first)
       (map-indexed (fn [i v] (* (inc i) (second v))))
       (apply +)))

(defn -main [day]
  (let [input (->> day file->str (re-seq #"\w+") (partition 2))]
    {:part1 (winnings input 1) :part2 (winnings input 2)}))


(comment
  (let [test-input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"
        input (partition 2 (re-seq #"\w+" test-input))]
    (assert (= 6440 (winnings input 1)))
    (assert (= 5905 (winnings input 2))))
  )
