(ns aoc2018.problem-1-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

(defn parse
  [file]
  (let [text (-> file io/resource slurp)
        lines (str/split-lines text)
        str->int #(Integer/parseInt %)]
    (map str->int lines)))

(defn solve-1-2
  "주어진 숫자열을 순서대로 덧셈하다 처음으로 두 번째로 나오는 수를 리턴합니다.
  https://adventofcode.com/2018/day/1 "
  ([numbers last-sum number-set]
   (let [next-sum (+ last-sum (first numbers))]
     (if (number-set next-sum)
       ; return
       next-sum
       (recur
         (rest numbers)
         next-sum
         (conj number-set next-sum)))))

  ([numbers] (solve-1-2 numbers 0 #{0})))


(solve-1-2 (cycle (parse "aoc2018/input1.txt")))


(defn solve-1-2-1
  [numbers]
  (loop [nums numbers
         sum 0
         sum-set #{}]
    (let [first-num (first nums)
          real-first-num (if (nil? first-num)
                           (first numbers)
                           first-num)
          real-nums (if (empty? nums)
                      (rest numbers)
                      (rest nums))
          next-sum (+ real-first-num sum)]
      (if (sum-set next-sum)
        (do
          (println next-sum)
          next-sum)
        (recur real-nums
               next-sum
               (conj sum-set next-sum)))
      )))

(solve-1-2-1 input)