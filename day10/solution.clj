(ns day10.solution
  (:require [tools :refer [file->str]]
            [clojure.set :refer [map-invert]]
            [clojure.string :refer [includes?]]))

(defn grid [input]
  (first
   (reduce
    (fn [[g [y x]] ch] [(assoc g [y x] (str ch)) (case ch \newline [(inc y) 0] [y (inc x)])])
    [(sorted-map) [0 0]] input)))

(def dirs {"S" [[-1 0] [1 0] [0 -1] [0 1]]
           "|" [[-1 0] [1 0]]
           "-" [[0 1] [0 -1]]
           "F" [[0 1] [1 0]]
           "J" [[-1 0] [0 -1]]
           "7" [[0 -1] [1 0]]
           "L" [[-1 0] [0 1]]})

(defn can-move? [point ch]
  (includes? ({[0 -1] "-LF" [0 1]  "-J7" [1 0]  "|LJ" [-1 0] "|7F"} point) ch))

(defn forward [point maze pipe]
  (first
   (for [pos (get dirs (maze point)),
         :let [pos' (mapv + point pos), ch (get maze pos' "X")]
         :when (not (contains? pipe pos'))
         :when (can-move? pos ch)]
     pos')))

(defn discover-pipe [start grid]
  (loop [pos start, pipe #{}]
    (if (nil? pos) pipe
        (recur (forward pos grid pipe), (conj pipe pos)))))

;; pretty useful to see how the pipe actually looks like (gaps!)
(defn show [grid pipe & [inside]]
  (println
   (apply str
    (for [[pos ch] grid]
      (cond
        (contains? pipe pos)
        ({"S" "S", "-" "━", "|" "┃", "L" "┗", "F" "┏", "J" "┛", "7" "┓"} ch)
        (contains? inside pos) "O"
        :else (case ch "\n" ch "."))))))

;; using a borrowed odd-even algo, which assumes an element is inside the loop
;; if there's an odd number of Z-turnings or horizontal veins above the point
;;  ┏ or ┓  or ━
;;  ┃    ┃    (x)
;;  ┛    ┗
;; (x)  (x)
(defn inside? [[y x :as pos] pipe grid]
  (when-not (contains? pipe pos)
    (let [marks (for [y' (range y)
                      :let [pos [y' x]]
                      :when (contains? pipe pos) :when (not= "|" (grid pos))]
                  (grid pos))
          veins (re-seq #"-|FJ|7L" (apply str marks))]
      (odd? (count veins)))))

(defn -main [day]
  (let [field (->> day file->str grid)
        start ((map-invert field) "S")
        pipe (discover-pipe start field)]
    {:part1 (do ;; (show field pipe)
                (quot (count pipe) 2))
     :part2
     (let [inside (filter #(inside? % pipe (assoc field start "|")) (keys field))]
       (show field pipe (set inside))
       (count inside))}))

(comment
  (let [test-input1 "7-F7-
.FJ|7
SJ.L7
|F--J
LJ.LJ"
        test-input2 "..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........
"
        start #((map-invert %) "S")
        maze1 (grid test-input1)
        maze2 (grid test-input2), s2 (start maze2)
        pipe2 (discover-pipe s2 maze2)]
    (assert (= 8 (quot (count (discover-pipe (start maze1) maze1)) 2)))
    (assert (= 4 (->> maze2 keys (filter #(inside? % pipe2 (assoc maze2 s2 "F"))) count))))
  )
