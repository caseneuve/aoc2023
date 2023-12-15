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

(comment
  ;; caveats for using `array-map`:
  (class (reduce #(assoc %1 %2 %2) (array-map) (range 8))) ;; => clojure.lang.PersistentArrayMap
  (class (reduce #(assoc %1 %2 %2) (array-map) (range 9))) ;; => clojure.lang.PersistentHashMap

  ;; as my input never creates maps larger than 6 keys at a time, it works!
  ;; coult be replaced with `ordered-map` from `flatland.ordered.map`
  ;; or we could implement alternative `assoc*` and `dissoc*` funcs
  ;; and operate on vectors instead of array-maps and update condition in `focusing-powers`:

  (defn assoc* [mx a b]
    (let [i (.indexOf (mapv first mx) a)]
      (if (< i 0) (conj mx [a b]) (assoc mx i [a b]))))

  (defn dissoc* [mx a]
    (let [i (.indexOf (mapv first mx) a)]
      (if (< i 0) mx (into [] cat [(subvec mx 0 i) (subvec mx (inc i))]))))
  )

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
