(ns day17.solution
  (:require [tools :refer [file->lines]]
            [clojure.string :refer [split-lines]]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [lines]
  (->> lines (map seq) (mapv #(mapv (comp parse-long str) %))))

(def L [0 -1])
(def R [0 1])
(def D [1 0])
(def U [-1 0])

(defn move [G [[pos dir nm] th] v-dir]
  (let [ndir
        (case v-dir
          :left  ({R U, U L, L D, D R} dir), :right ({R D, D L, L U, U R} dir), dir)
        npos (mapv + pos ndir)
        h (get-in G npos)]
    (when h [[npos ndir (case v-dir :forward (inc nm) 1)] (+ th h)])))

(defn moves [r1 r2 G [[_ _ nm] _ :as state]]
  (remove nil?
          [(when (> nm r1) (move G state :left))
           (when (> nm r1) (move G state :right))
           (when (< nm r2) (move G state :forward))]))

(defn solve [G r1 r2]
  (let [Y (dec (count G)), X (dec (count (first G)))]
    (loop [q (priority-map [[0 0] R 0] 0, [[0 0] D 0] 0), seen {}]
      (let [[[pos _ _ :as key] hl :as state] (peek q)]
        (cond (= pos [Y X]) hl
              (contains? seen key) (recur (pop q) seen)
              :else (let [new (remove
                               (fn [[state ht]]
                                 (or (>= ht (q state Long/MAX_VALUE))
                                     (>= ht (seen state Long/MAX_VALUE))))
                               (moves r1 r2 G state))]
                      (recur (into (pop q) new) (assoc seen key hl))))))))

(defn -main [day]
  (let [input (->> day file->lines parse)]
    {:part1 (solve input 0 3)
     :part2 (solve input 3 10)}))


(comment
  (let [test-input "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
"
        G (->> test-input split-lines parse)]
    (assert (= 102 (solve G 0 3)))
    (assert (=  94 (solve G 3 10))))
  )
