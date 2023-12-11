(ns day11.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split-lines]]
            [clojure.math.combinatorics :as combo]))

(defn shifts [input exp]
  (let [lx (split-lines input)
        xf (map #(if (every? #{\.} %) exp 1))]
    {:rows (->> lx (transduce xf conj) (reductions + 0))
     :cols (->> lx (apply mapv str) (transduce xf conj) (reductions + 0) cycle)}))

(defn galaxies [input expansion]
  (let [{:keys [rows cols]} (shifts input expansion)]
    (first
     (reduce
      (fn [[gx [r c]] ch]
        (cond (= ch \#)       [(conj gx [(first r) (first c)]) [r (next c)]]
              (= ch \newline) [gx [(next r) (next c)]]
              :else           [gx [r (next c)]]))
      [[] [rows cols]] input))))

(defn pairs [xs] (combo/combinations xs 2))

(defn manhattan [coords] (->> coords (apply map (comp abs -)) (apply +)))

(defn -main [day]
  (let [input (file->str day)
        solve-with-expansion #(->> % (galaxies input) pairs (map manhattan) (apply +))]
    {:part1 (solve-with-expansion 2)
     :part2 (solve-with-expansion 1000000)}))


(comment
  (let [test-input "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"
        solve-with-expansion #(->> % (galaxies test-input) pairs (map manhattan) (apply +))]
    (assert (=  374 (solve-with-expansion   2)))
    (assert (= 1030 (solve-with-expansion  10)))
    (assert (= 8410 (solve-with-expansion 100))))
  )
