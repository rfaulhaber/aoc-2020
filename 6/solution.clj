(ns solution
  (:require [clojure.string :as str])
  (:require [clojure.core.reducers :as r]))

(def input (map (fn [row] (str/replace row #"\n" "")) (str/split (slurp "./input.txt") #"(\n\n)")))

; TODO why doesn't this work for fold?
(defn count-unique-answers [rows]
  (r/reduce
   (fn [sum row]
     (+ sum (count row)))
   0
   (map distinct rows)))


(println "Part 1: " (count-unique-answers input))
