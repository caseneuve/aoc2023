(ns day01.solution
  (:require [clojure.edn :refer [read-string]]
            [clojure.string :refer [join split-lines index-of last-index-of]]
            [tools :refer [file->lines]]))

(def words-and-digits
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})

(defn first-last [coll s]
  (->> coll
       (keep #(when-let [io (index-of s %)]
                {io (get words-and-digits % %), (last-index-of s %) (get words-and-digits % %)}))
       (into (sorted-map))
       ((comp read-string join (juxt first last) vals))))

(defn -main [day]
  (let [lines (file->lines day)
        p1 (partial first-last (vals words-and-digits))
        p2 (partial first-last (flatten (vec words-and-digits)))]
    {:part1 (apply + (map p1 lines))
     :part2 (apply + (map p2 lines))}))


(comment
  (let [input1 "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"
        input2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"]
    (assert (= 142 (->> input1 split-lines (map #(first-last (vals words-and-digits) %)) (apply +))))
    (assert (= 281 (->> input2 split-lines (map #(first-last (flatten (vec words-and-digits)) %)) (apply +))))

    ;; alternative solution using overlapping regex
    (->> input2
         split-lines
         (map #(re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))" %))
         ;; etc...
         ))
  )
