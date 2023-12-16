(ns day16.solution
  (:require [tools :refer [file->str transpose]]))

(defn grid [input]
  (first
   (reduce
    (fn [[g [y x]] ch] (case ch \newline [g [(inc y) 0]], [(assoc g [y x] ch) [y (inc x)]]))
    [{} [0 0]] input)))

(def up    [-1  0])
(def down  [ 1  0])
(def right [ 0  1])
(def left  [ 0 -1])

(defn dirs [dir char]
  (case char
    \\ ({right [down   ], up [left      ], left [up     ], down [right     ]} dir)
    \- ({right [right  ], up [left right], left [left   ], down [right left]} dir)
    \| ({right [up down], up [up        ], left [up down], down [down      ]} dir)
    \/ ({right [up     ], up [right     ], left [down   ], down [left      ]} dir)
    [dir]))

(defn moves [pos dirs] (mapv #(conj [pos] %) dirs))

(defn energize [G from to]
  (let [initial-dirs (dirs to (G from))]
    (loop [Q (into clojure.lang.PersistentQueue/EMPTY (moves from initial-dirs))
           seen {from (into #{} initial-dirs)}]
      (if (empty? Q) (count seen)
          (let [[pos dir] (peek Q), new-pos (mapv + pos dir), ch (G new-pos)
                seen? (contains? (seen new-pos) dir)]
            (if (or seen? (nil? ch))
              (recur (pop Q) seen)
              (recur (into (pop Q) (moves new-pos (dirs dir ch)))
                     (update seen new-pos (comp set conj) dir))))))))

(defn max-energy [G]
  (let [[Y X] (->> G keys transpose (map #(apply max %)))
        results [(pmap #(energize G [0 %] down ) (range (inc X)))
                 (pmap #(energize G [Y %] up   ) (range (inc X)))
                 (pmap #(energize G [% 0] right) (range (inc Y)))
                 (pmap #(energize G [% X] left ) (range (inc Y)))]]
    (apply max (into [] cat results))))

(defn -main [day]
  (let [G (->> day file->str grid)]
    {:part1 (energize G [0 0] right)
     :part2 (max-energy G)}))


(comment
  (let [
        test-input ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....
"
        G (->> test-input grid)]
    (assert (= 46 (energize G [0 0] right)))
    (assert (= 51 (max-energy G))))
  )
