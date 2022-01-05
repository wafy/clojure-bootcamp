(ns aoc2018.day5-2
  (:require [clojure.string :as str]
            [aoc2018.day5-1 :refer :all]))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(def sample-input-string "dabAcCaCBAcCcaDA")

; 대문자로 변환하여 빈도수의 값을 반환
(defn  strings->upper-keys
  [input-strings]
  (->> input-strings
       (str/upper-case)
       frequencies
       keys))

(comment
  (strings->upper-keys sample-input-string))

(defn remove-char
  [input-strings ch]
  (str/replace input-strings (re-pattern (str "(?i)" ch)) ""))

(defn solve2 [input-strings]
  (->> (strings->upper-keys input-strings)
       (map #(solve (remove-char input-strings %)))
       (apply min)))

(comment
  ;4
  (solve2 sample-input-string)
  ;4098
  (solve2 input-string))


