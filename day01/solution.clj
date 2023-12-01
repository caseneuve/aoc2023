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

(defn first-last->num [tokens s]
  (->> tokens
       (keep #(when-let [io (index-of s %)]
                {io (get words-and-digits % %), (last-index-of s %) (get words-and-digits % %)}))
       (into (sorted-map))
       ((comp read-string join (juxt first last) vals))))

(defn -main [day]
  (let [solve (fn [c] (->> day file->lines (map #(first-last->num c %)) (apply +)))]
    {:part1 (solve (vals words-and-digits))
     :part2 (solve (flatten (vec words-and-digits)))}))


(comment
  (let [input1 "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
        input2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
        solve (fn [i c] (->> i split-lines (map #(first-last->num c %)) (apply +)))]
    (assert (= 142 (solve input1 (vals words-and-digits))))
    (assert (= 281 (solve input2 (flatten (vec words-and-digits)))))

    ;; alternative solution using overlapping regex
    (->> input2
         split-lines
         (map #(re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))" %))
         ;; etc...
         ))
  )
