(ns day09.solution
  (:require [tools :refer [file->lines]]
            [clojure.string :refer [split-lines]]))

(defn parse [lines]
  (->> lines (map #(re-seq #"-*\d+" %)) (map (partial map parse-long))))

(defn reductor [line]
  (->> line (partition 2 1) (map (fn [[a b]] (- b a)))))

(defn predict [report]
  (->> report
       (iterate reductor)
       (take-while #(not (every? zero? %)))
       (map last) reverse (reduce + 0)))

(defn -main [day]
  (let [input (->> day file->lines parse)]
    {:part1 (transduce (map predict) + input)
     :part2 (transduce (comp (map reverse) (map predict)) + input)}))

(comment
  (let [test-input "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"
        input (->> test-input split-lines parse)]
    (assert (= 114 (transduce (map predict) + input)))
    (assert (= 2 (transduce (comp (map reverse) (map predict)) + input))))
  )
