(ns day04.solution
  (:require [tools :refer [file->lines]]
            [clojure.string :refer [split-lines]]
            [clojure.math :refer [pow]]
            [clojure.set :refer [intersection]]))

(defn parse-line [idx l]
  (->> l (re-seq #"\d+") rest (map parse-long) (split-at idx) (map set) (apply intersection) count))

(defn p1 [input]
  (map #(int (pow 2 (dec %))) input))

(defn p2 [input]
  (reduce
   (fn [s-cards matching] (conj s-cards (reduce + 1 (take matching s-cards))))
   '() (reverse input)))                ;; going backwards is the key for optimization,
                                        ;; that's a refactored version of my initial brute "forward" solution

(defn -main [day]
  (let [solve-with #(->> day file->lines (map (partial parse-line 10)) % (apply +))]
    {:part1 (solve-with p1)
     :part2 (solve-with p2)}))


(comment
  (let [test-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
        solve-with #(->> test-input split-lines (map (partial parse-line 5)) % (apply +))]
    (assert (= 13 (solve-with p1)))
    (assert (= 30 (solve-with p2))))
  )
