(ns solution
  (:require [clojure.string :as str])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.set :as s])
  )

(def input-part-1 (parse-input (slurp "./input.txt") :flat true))
(def practice-part-1 (parse-input (slurp "./practice.txt") :flat true))
(def input-part-2 (parse-input (slurp "./input.txt")))
(def practice-part-2 (parse-input (slurp "./practice.txt")))

; TODO this seems contrived, improve?
(defn parse-input [input & {:keys [flat]}]
  (if flat
    (map
     (fn [row]
       (mapcat
        (fn [r]
          (str/split r #""))
        (str/split row #"\n")))
     (str/split input #"(\n\n)"))
    (map
     (fn [row]
       (str/split row #"\n"))
     (str/split input #"(\n\n)"))))


; part 1
; TODO why doesn't this work for r/fold?
(defn count-unique-answers [rows]
  (r/reduce
   (fn [sum row]
     (+ sum (count (set row))))
   0
   rows))

; part 2
; there has got to be a better way to do this
(defn count-every-answer [rows]
  (reduce
   +
   (map count
        (map
         (fn [l]
           (apply s/intersection l))
   (map
    (fn [row]
      (map
       (fn [st]
         (set (str/split st #"")))
       row))
   rows)))))


(println "Part 1 (practice): " (count-unique-answers practice-part-1))
(println "Part 1: " (count-unique-answers input-part-1))
(println "Part 2 (practice): " (count-every-answer practice-part-2))
(println "Part 2: " (count-every-answer input-part-2))
