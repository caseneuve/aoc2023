(ns day13.solution
  (:require [tools :refer [file->str transform]]
            [clojure.string :refer [split-lines split]]))

(defn scan-line [line]
  (for [i (range 1 (count line)), :let [[L R] (split-at i line)]]
    (apply + (map (comp #(if (apply = %) 0 1) vector) (reverse L) R))))

(defn reflection [part]
  (fn [block]
    (->> block (map scan-line) transform
         (keep-indexed (fn [i xs] (when (= (reduce + xs) (dec part)) (inc i))))
         first)))

(defn summerize [blocks f]
  (+ (transduce (keep f) + blocks)
     (* 100 (transduce (comp (map transform) (keep f)) + blocks))))

(defn -main [day]
  (let [input (->> (split (file->str day) #"\n\n") (map split-lines))]
    {:part1 (summerize input (reflection 1))
     :part2 (summerize input (reflection 2))}))


(comment
  (let [test-input "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"
        input (->> (split test-input #"\n\n") (map split-lines))]
    (assert (= 405 (summerize input (reflection 1))))
    (assert (= 400 (summerize input (reflection 2)))))
  )
