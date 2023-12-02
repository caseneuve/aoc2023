(ns day02.solution
  (:require [tools :refer [file->lines]]
            [clojure.string :refer [split-lines split replace]]
            [clojure.edn :refer [read-string]]))

(defn games [s]
  (->> (-> s (split #":") second (replace #"(\d+) (\w+)" "{$2 $1}") (split #"[,;]"))
       (map read-string) (apply merge-with max) (into (sorted-map))))

(def p1
  (let [rules (sorted-map "red" 12 "green" 13 "blue" 14)
        possilble? #(every? true? (map <= (vals %) (vals rules)))]
    #(keep-indexed (fn [id game] (when (possilble? game) (inc id))) %)))

(def p2 (fn [i] (->> i (map vals) (map #(apply * %)))))

(defn -main [day]
  (let [solve-with #(->> day file->lines (map games) % (apply +))]
    {:part1 (solve-with p1) :part2 (solve-with p2)}))


(comment
  (let [test-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        solve-with #(->> test-input split-lines (map games) % (apply +))]
    (assert (= 8 (solve-with p1)))
    (assert (= 2286 (solve-with p2))))
  )
