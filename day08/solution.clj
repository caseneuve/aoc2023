(ns day08.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [ends-with?]]
            [clojure.math.numeric-tower :refer [lcm]]))

(defn parse [s]
  (let [[i & m] (re-seq #"\w+" s)]
    [(map {\L first \R second} i)
     (into {} (comp (partition-all 3) (map (juxt first rest))) m)]))

(defn steps->z [[instructions network] start pred]
  (loop [[i & r] (cycle instructions), s 0, cur start]
    (if (pred cur) s
        (recur r (inc s) (i (network cur))))))

(defn steps->all-z [[_ network :as input]]
  (->> network keys (filter #(ends-with? % "A"))
       (pmap (fn [start] (steps->z input start #(ends-with? % "Z"))))
       (reduce lcm)))

(defn -main [day]
  (let [input (->> day file->str parse)]
    {:part1 (steps->z input "AAA" #(= "ZZZ" %))
     :part2 (steps->all-z input)}))


(comment
  (let [test-input1 (-> "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)" parse)
        test-input2 (-> "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)" parse)]
    (assert (= 6 (steps->z test-input1 "AAA" #(= "ZZZ" %))))
    (assert (= 6 (steps->all-z test-input2))))
  )
