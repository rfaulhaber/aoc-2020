(ns solution
  (:require [clojure.string :as str]))

(def input (slurp "./input.txt"))
(def practice (slurp "./practice.txt"))

(defn parse-input [input]
  (let [lines (str/split input #"\n")]
    (map read-string lines)))

; thank you stackoverflow!
(defn pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s]
                [(first coll) y])
              (pairs s))))

(defn sum [l]
  (reduce + l))

; seems inefficient, can this be improved?
(defn find-run [target input]
  (loop [n 2
         run (take 2 input)]
    (cond
      (= target (sum run)) run
      (> (sum run) target) (find-run target (next input))
      :else (recur (inc n) (take (inc n) input)))))

(defn xmas-valid? [preamble val]
  (let [p (set (map (partial apply +) (pairs preamble)))]
    (contains? p val)))

(defn part-1 [preamble-len input]
  (let [[preamble rs] (split-at preamble-len input)
        val (first rs)]
    (if (not (xmas-valid? preamble val))
      val
      (part-1 preamble-len (next input)))))

(defn part-2 [target input]
  (let [run (find-run target input)
        min (apply min run)
        max (apply max run)]
    (+ min max)))

(def parsed-input (parse-input input))
(def target (part-1 25 parsed-input))

(printf "Part 1: %d\n" target)
(printf "Part 2: %d\n" (part-2 target parsed-input))
