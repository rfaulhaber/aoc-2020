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

(defn part-1 [preamble-len input]
  (let [[preamble rs] (split-at preamble-len input)
        val (first rs)]
    (if (not (xmas-valid? preamble val))
      val
      (part-1 preamble-len (next input)))))

;; (defn part-2 [target input]
;;   (if (> target (first input))
;;     (part-2 target (next input))
;;     (loop [sum 0
;;            idx 0]
;;       (let [val (first input)]
;;       (if (> (+ sum val) target)
;;         (part-2 target (second (split-at idx input)))
;;         (recur )
;;         )
;;         )
;;       )
;;     )
;;   )

(defn xmas-valid? [preamble val]
  (let [p (set (map (partial apply +) (pairs preamble)))]
    (contains? p val)))

(println "Part 1: " (part-1 25 (parse-input input)))
