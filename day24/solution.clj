(ns day24.solution
  (:require [tools :refer [file->str]]
            [clojure.math.combinatorics :refer [combinations]]))

(defn ->abc
  "Point in time (t), knowing pos (x, y) and velocties (v):
  [px py] = [x y] + t[vx vy]
  px = x + tvx => t = (px - x)/vx
  py = y + tvy => t = (py - y)/vy
  vy(px - x) = vx(py - y)
  vypx - vxpy = vyx - vxy
  a ?    b ?    c"
  [[x y _ vx vy _]]
  ;; a   b      c
  [  vy  (- vx) (- (* vy x) (* vx y))])

(defn parallel?
  "Solving for:
  a1px + b1py = c1
  a2px + b2py = c2
  so:
  x = (c1b2 - c2b1)/(a1b2 - a2b1)
  y = (c2a1 - c1a2)/(a1b2 - a2b1)
  If a1b2 = a2b1, we get division by 0, so x, y are undefined
  and there's no intersection == lines are parallel"
  [[[a1 b1 _] [a2 b2 _]]]
  (=  (* a1 b2) (* a2 b1)))

(defn intersection
  "See equations above"
  [[[a1 b1 c1] [a2 b2 c2]]]
  [(/ (- (* c1 b2) (* c2 b1)) (- (* a1 b2) (* a2 b1)))
   (/ (- (* c2 a1) (* c1 a2)) (- (* a1 b2) (* a2 b1)))])

(defn part1 [input MIN MAX]
  (for [[h1 h2 :as ls] (combinations input 2)
        :let [lsc (map ->abc ls)]
        :when (not (parallel? lsc))
        :let [[px py] (intersection lsc)
              [x1 y1 _ vx1 vy1 _] h1
              [x2 y2 _ vx2 vy2 _] h2]
        ;; check if within boundaries
        :when (and (<= MIN px MAX) (<= MIN py MAX))
        ;; check if not in the future
        ;; if velocity is positive, px - x has to be positive
        ;; if velocity is negative, px - x has to be negative
        :when (and (>= (* (- px x1) vx1) 0) (>= (* (- py y1) vy1) 0)
                   (>= (* (- px x2) vx2) 0) (>= (* (- py y2) vy2) 0))]
    1))

(defn -main [day]
  (let [input (->> day file->str (re-seq #"-?\d+") (map bigint) (partition 6))]
    {:part1 (apply + (part1 input 200000000000000 400000000000000))
     :part2 "Check solution.py :)"}))


(comment
  (let [test-input "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"
        input (->> test-input (re-seq #"-?\d+") (map bigint) (partition 6))]
    (assert (= 2 (apply + (part1 input 7 27)))))
  )
