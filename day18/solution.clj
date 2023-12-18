(ns day18.solution
  (:require [tools :refer [file->lines manhattan]]
            [clojure.string :refer [split-lines]]))

(defn parse [l]
  (let [[_ dir1 num _ hex dir2] (re-matches #"(\w) (\d+) \(#((\w{5})(\w))\)" l)]
    {:1 [dir1 (parse-long num)]
     :2 [({"0" "R" "1" "D" "2" "L" "3" "U"} dir2) (read-string (str "0x" hex))]}))

(defn edges [input ]
  (reduce
   (fn [acc [d n _]]
     (conj acc
           (let [[y x] (peek acc)]
             (case d
               "R" [y (+ x n )]
               "D" [(+ y n) x]
               "L" [y (- x n)]
               "U" [(- y n) x]))))
   [[0 0]] input))

;; shoelace formula: https://en.wikipedia.org/wiki/Shoelace_formula
(defn lava-vol [input]
  (let [pairs (partition 2 1 input)
        shoelace (reduce (fn [acc [[y x] [y' x']]] (+ acc (/ (- (* y' x) (* y x')) 2))) 0 pairs)
        circum (reduce + (map #(apply manhattan %) pairs))]
    (inc (+ shoelace (/ circum 2)))))

(defn -main [day]
  (let [solve-for #(->> day file->lines (map parse) (map %) edges lava-vol)]
    {:part1 (solve-for :1)
     :part2 (solve-for :2)}))


(comment
  (let [test-input "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"
        solve-for #(->> test-input split-lines (map parse) (map %) edges lava-vol)]
    (assert (=           62 (solve-for :1)))
    (assert (= 952408144115 (solve-for :2))))
  )
