(ns tools
  (:require [clojure.string :refer [split-lines]]
            [clojure.test :refer [deftest testing is run-tests]]))

(defn file->str [dir] (slurp (str dir "/" "input.txt")))
(defn file->lines [dir] (split-lines (file->str dir)))
(defn str->ints [s] (->> s (re-seq #"-*\d+") (map parse-long)))


(comment
  (deftest str->ints-test
    (testing "transforms string into list of ints"
      (is (= '(1 3 2 4) (str->ints "1 asd 3 (QWER 2: 4)."))))
    (testing "is negative numbers aware"
      (is (= '(-1 3 -2 4) (str->ints "-1 3 -2 4"))))
    (testing "returns empty list when no ints found"
      (is (= () (str->ints "no ints")))))

  (assert (->> (run-tests 'tools) :fail zero?))
  )
