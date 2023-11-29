(ns tools
  (:require [clojure.string :refer [split-lines]]))

(defn file->str [dir] (slurp (str dir "/" "input.txt")))
(defn file->lines [dir] (split-lines (file->str dir)))

