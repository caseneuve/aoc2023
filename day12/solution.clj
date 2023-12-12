(ns day12.solution
  (:require [tools :refer [file->lines str->ints]]
            [clojure.string :refer [split-lines split join]]))

;; TODO: try a different approach to play more with dynamic programming
;; below's a borrowed solution (sligthly changed and commented)

(defn parse [l] (let [[p ns] (split l #" ")] [p (str->ints ns)]))

(defn tails [l n]
  (for [i (range (inc (- (count l) n)))
        :while (every? #{\. \?} (take i l))            ; preceding chars: only . or ? (or nothing: ^)
        :when  (every? #{\# \?} (take n (drop i l)))   ; pattern we look for
        :when  (contains? #{\. \?} (nth l (+ i n) \.)) ; succeeding chars: . or ? (or nothing: $)
        ]
    (drop (+ i n 1) l)))

(defn arrangements [row numbers]
  (if-let [[n & nrs] (seq numbers)]
    (reduce + (for [s (tails row n)] (arrangements s nrs)))
    (if (every? #{\. \?} row) 1 0)      ; will return 1 if empty seq (see below)
    ))

(comment
  (every? #{\. \?} '()) ;; => true
  )

(def arrangements (memoize arrangements))

(defn unfold [[p n]]
  [(join \? (repeat 5 p)) (flatten (repeat 5 n))])


(defn -main [day]
  (let [input (->> day file->lines (map parse))
        solve-with #(->> input % (pmap (partial apply arrangements)) (apply +))]
    {:part1 (solve-with identity), :part2 (solve-with (partial map unfold))}))


(comment
  (let [test-input "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"
        input (->> test-input split-lines (map parse))
        solve-with #(->> input % (pmap (partial apply arrangements)) (apply +))]
    (assert (=     21 (solve-with identity)))
    (assert (= 525152 (solve-with (partial map unfold)))))
  )
