(ns day06.solution
  (:require [tools :refer [file->lines]]
            [clojure.string :refer [split-lines]]))

(defn parse [nums p]
  (case p
    1 (->> nums (map (partial map parse-long)) (apply mapv vector))
    2 (->> nums (map (partial apply str)) (map parse-long))))

(defn wins [[time distance]]
  (count (keep #(when (> (* (- time %) %) distance) %) (range 1 time))))

(defn -main [day]
  (let [input (partial parse (->> day file->lines (map (partial re-seq #"\d+"))))]
    {:part1 (->> (input 1) (map wins) (apply *))
     ;; POV: initially thought about binary search, but after reading the input,
     ;; chose violence and went with brute-forcing it...
     ;; still, BS is a TODO here, if I'd find more time
     :part2 (wins (input 2))}))


(comment
  (let [test-input "Time:      7  15   30\nDistance:  9  40  200\n"
        input (partial parse (->> test-input split-lines (map (partial re-seq #"\d+"))))]
    (assert (= 288 (->> (input 1) (map wins) (apply *))))
    (assert (= 71503 (wins (input 2)))))
  )
