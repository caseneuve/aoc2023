(ns day19.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [split-lines split index-of]]))

(defn parse [input]
  (let [[workflows ratings] (->> (split input #"\n\n") (map split-lines))
        parse-workflow
        (fn [wkf-s]
          (let [[_ name rules-str] (re-find #"(\w+)\{(.*)\}" wkf-s)
                rules (split rules-str #",")
                parse-rule (fn [v-s]
                             (let [[_ l op n reg fb] (re-find #"([xmas])([<>=])(\d+):(\w+)|(\w+)" v-s)]
                               (if fb [fb] [reg op (index-of "xmas" l) (parse-long n)])))]
            {name (map parse-rule rules)}))]
    [(into {} (map parse-workflow) workflows)
     (into [] (map (fn [s] (->> s (re-seq #"[-]*\d+") (mapv parse-long)))) ratings)]))

;; part1
(defn process-workflow [workflows xmas key]
  (loop [workflows (workflows key)]
    (let [[K op idx n] (first workflows)]
      (if (nil? op) K
          (let [N (xmas idx)]
            (if (({">" > "<" <} op) N n) K (recur (next workflows))))))))

(defn sum-rating [workflows xmas]
  (loop [k "in"]
    (case k
      "A" (apply + xmas)
      "R" 0
      (recur (process-workflow workflows xmas k)))))

;; part2
(defn keys->ranges [workflows [key' xmas]]
  (loop [rules (workflows key'), xs xmas, ranges []]
    (let [[key op idx n] (first rules)]
      (if (nil? op) (conj ranges [key xs])
          (let [[rmin rmax] (xs idx)]
            (cond
              (> rmin n)  (conj ranges [key xs])
              (<= rmax n) (recur (next rules) xs ranges)
              :else
              (let [[nxt fin] (case op "<" [[n rmax] [rmin (dec n)]], [[rmin n] [(inc n) rmax]])]
                (recur (next rules), (assoc xs idx nxt), (conj ranges [key (assoc xs idx fin)])))))))))

(defn sum-ranges [workflows]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY [["in" (vec (repeat 4 [1 4000]))]]), done []]
    (cond (empty? q) (->> done
                          (map second)
                          (map #(map (fn [[a b]] (inc (- b a))) %))
                          (map #(apply * %))
                          (apply +))
          :else (let [xs (peek q)
                      cand (remove #(= "R" (first %)) (keys->ranges workflows xs))
                      done (into done (filter #(= "A" (first %)) cand))
                      rst (remove #(= "A" (first %)) cand)]
                  (recur (into (pop q) rst), done)))))

(defn -main [day]
  (let [[WORKFLOWS RATINGS] (->> day file->str parse)]
    {:part1 (->> RATINGS (map (partial sum-rating WORKFLOWS)) (apply +))
     :part2 (sum-ranges WORKFLOWS)}))


(comment
  (let [test-input "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"
        [W R] (parse test-input)]
    (assert (= 19114 (->> R (map (partial sum-rating W)) (apply +))))
    (assert (= 167409079868000 (sum-ranges W))))
  )
