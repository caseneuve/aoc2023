(ns day03.solution
  (:require [tools :refer [file->str]]))

(defn grid [input]
  (first
   (reduce
    (fn [[g [y x]] ch]
      (if (= ch \newline) [g [(inc y) 0]], [(assoc g [y x] ch) [y (inc x)]]))
    [(sorted-map) [0 0]] input)))

(defn adjacent [point]
  (for [x [-1 0 1] y [-1 0 1] :when (not= [x y] [0 0])] (mapv + [x y] point)))

(defn digit? [c] (java.lang.Character/isDigit c))

;; Assumptions that worked:
;; - number can be adjacent only to one symbol
;; - there are no other symbols than * adjacent to multiple numbers
(defn number-groups [gr]
  (loop [[k & keys] (keys gr), acc {}, digits [], symbols #{}]
    (cond
      (nil? k) (vals acc),
      (not (digit? (gr k)))
      (recur keys (reduce #(update %1 %2 conj (parse-long (apply str digits))) acc symbols) [] #{}),
      :else
      (let [xf (filter #(when-let [c (gr %)] (not (or (= \. c) (digit? c)))))]
        (recur keys acc (conj digits (gr k)) (into symbols xf (adjacent k)))))))

(defn -main [day]
  (let [data (->> day file->str grid number-groups)
        solve-with #(->> data % (apply +))]
    {:part1 (solve-with flatten)
     :part2 (solve-with (fn [v] (keep #(when (= (count %) 2) (apply * %)) v)))}))


(comment
  (let [test-input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
        solve-with #(->> test-input grid number-groups % (apply +))]
    (assert (=   4361 (solve-with flatten)))
    (assert (= 467835 (solve-with (fn [v] (keep #(when (= (count %) 2) (apply * %)) v)))))
    )
  )
