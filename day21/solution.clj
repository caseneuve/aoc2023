(ns day21.solution
  (:require [tools :refer [file->lines]]
            [clojure.string :refer [index-of split-lines]]
            [clojure.math :refer [floor pow]]))

(defn adjacent [G point]
  (for [dir [[-1 0] [1 0] [0 -1] [0 1]]
        :let [[r c :as new] (mapv + dir point)]
        :when (contains? #{\. \S} (get-in G [r c]))]
    new))

(defn fill [G S steps]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY [[S steps]]), filled #{}, seen #{S}]
    (if (empty? q) (count filled)
        (let [[p s] (peek q)
              filled (cond-> filled (zero? (mod s 2)) (conj p))]
          (if (zero? s) (recur (pop q) filled seen)
              (let [new (for [np (adjacent G p) :when (not (contains? seen np))] [np (dec s)])]
                (recur (into (pop q) new) filled (into seen (map first) new))))))))

(defn part2 [G S]
  (let [[sr sc] S
        steps 26501365

        G-size      (count G)
        total-width (dec (floor (/ steps G-size)))

        odd-tiles     (pow (inc (* (floor (/ total-width 2)) 2)) 2)
        odd-tile-pts  (fill G S (inc (* G-size 2)))
        even-tiles    (pow (* (floor (/ (inc total-width) 2)) 2) 2)
        even-tile-pts (fill G S (* G-size 2))

        corner-steps  (dec G-size)
        corner-top    (fill G [(dec G-size) sc           ] corner-steps)
        corner-right  (fill G [sr           0            ] corner-steps)
        corner-bottom (fill G [0            sc           ] corner-steps)
        corner-left   (fill G [sr           (dec G-size) ] corner-steps)

        small-steps        (dec (floor (/ G-size 2)))
        small-top-right    (fill G [(dec G-size) 0            ] small-steps)
        small-top-left     (fill G [(dec G-size) (dec G-size) ] small-steps)
        small-bottom-right (fill G [0            0            ] small-steps)
        small-bottom-left  (fill G [0            (dec G-size) ] small-steps)

        large-steps        (dec (floor (/ (* G-size 3) 2)))
        large-top-right    (fill G [(dec G-size) 0            ] large-steps)
        large-top-left     (fill G [(dec G-size) (dec G-size) ] large-steps)
        large-bottom-right (fill G [0            0            ] large-steps)
        large-bottom-left  (fill G [0            (dec G-size) ] large-steps)]

    (bigint
     (+ (* odd-tiles odd-tile-pts)
        (* even-tiles even-tile-pts)
        (* (inc total-width) (+ small-top-right small-top-left small-bottom-left small-bottom-right))
        (* total-width       (+ large-bottom-left large-bottom-right large-top-left large-top-right))
        corner-top corner-right corner-bottom corner-left))))

(defn -main [day]
  (let [G (file->lines day)
        S (first (keep-indexed (fn [r l] (when-let [c (index-of l \S)] [r c])) G))]
    {:part1 (fill G S 64), :part2 (part2 G S)}))


(comment
  (let [test-input "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"
        G (->> test-input split-lines)
        S (first (keep-indexed (fn [r l] (when-let [c (index-of l \S)] [r c])) G))]
    (assert (= 16 (fill G S 6))))
  )
