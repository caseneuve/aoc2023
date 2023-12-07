(ns day07.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [index-of]]))

(defn typ [h part]
  (let [f (frequencies h)
        jok (if (= part 2) (get f \J 0) 0)
        frqs (sort > (vals (cond-> f (= 2 part) (dissoc \J))))]
    (cond
      (or (= 5 jok) (= (+ jok (first frqs)) 5))            7
      (= 4 (+ jok (first frqs)))                           6
      (and (= 3 (+ jok (first frqs))) (= 2 (second frqs))) 5
      (= 3 (+ jok (first frqs)))                           4
      (= '(2 2) (take 2 frqs))                             3
      (= 2 (+ jok (first frqs)))                           2
      (= 5 (count frqs))                                   1
      :else                                                0)))

(defn rank [h part]
  (let [deck (apply str (reverse (if (= part 2) "AKQT98765432J" "AKQJT98765432")))]
    (-> (map #(index-of deck %) h) (conj (typ h part)) vec)))

(defn winnings [input part]
  (->> input
       (map (fn [[hand bid]] [(rank hand part) (parse-long bid)]))
       (sort-by first compare)
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
