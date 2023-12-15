(ns day15.solution
  (:require [tools :refer [file->str]]
            [clojure.string :refer [trim-newline]]))

(defn hash* [cx] (reduce (fn [acc ch] (rem (* 17 (+ acc (int ch))) 256)) 0 cx))

(defn lenses [input]
  (reduce
   (fn [acc [lab foc]]
     (update acc (hash* lab) (if foc #(assoc % lab foc) #(dissoc % lab))))
   (zipmap (range 256) (repeat (array-map)))
   (map #(re-seq #"\w+" %) input)))

(defn focusing-powers [lenses]
  (keep
   (fn [[box slots]]
     (when-let [vs (vals slots)]
       (apply + (map-indexed (fn [i v] (* (inc box) (inc i) (parse-long v))) vs))))
   lenses))

(defn -main [day]
  (let [input (->> day file->str trim-newline (re-seq #"[^,]+"))]
    {:part1 (apply + (map hash* input))
     :part2 (apply + (focusing-powers (lenses input)))}))


(comment
  (let [test-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
"
        input (->> test-input trim-newline (re-seq #"[^,]+"))]
    (assert (= 1320 (apply + (map hash* input))))
    (assert (=  145 (apply + (focusing-powers (lenses input))))))
  )
