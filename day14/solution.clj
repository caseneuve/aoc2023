(ns day14.solution
  (:require [tools :refer [file->lines transpose]]
            [clojure.string :refer [split-lines]]))

(defn tilt [transform line]
  (loop [l (transform line), new []]
    (cond (empty? l) (transform new)
          (= (first l) \#) (recur (rest l) (conj new (first l)))
          :else (let [[head tail] ((juxt take-while drop-while) #(not= % \#) l)]
                  (recur tail (into new (reverse (sort head))))))))

(def tilt (memoize tilt))
(def tilt-nw (partial tilt identity))
(def tilt-se (partial tilt reverse))

(defn get-load [platform]
  (let [loads (map #(->> % reverse (map-indexed (fn [i ch] (if (= ch \O) (inc i) 0))) (reduce +)))]
    (transduce loads + platform)))

(defn cycle-around [platform]
  (->> platform
       (map tilt-nw) transpose (map tilt-nw) transpose
       (map tilt-se) transpose (map tilt-se) transpose))

;; Heurestics for p. 2:
;; After some time cycles repeat, it's only matter of getting the frequency
;; I solved it with printing and pencil:
;; - first 153 full tilts (N+W+S+E) are not forming any cycle,
;; - then there's a 26 element cycle that repeats forever
(defn -main [day]
  (let [input (->> day file->lines transpose)
        spinned (->> input (iterate cycle-around) (drop 153) (map get-load))]
    {:part1 (get-load (map tilt-nw input))
     :part2 (nth spinned (mod (- 1000000000 153) 26))}))


(comment
  (let [test-input "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
"
        input (->> test-input split-lines transpose)
        spinned (map get-load (iterate cycle-around input))]
    (assert (= 136 (get-load (map tilt-nw input))))
    ;; In the given example, cycles are 28 long, the first one being irregular
    (assert (=  64 (nth spinned (mod 1000000000 28)))))
  )
