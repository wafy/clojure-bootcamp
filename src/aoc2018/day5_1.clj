(ns aoc2018.day5-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabA
;; cCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(def input-string (-> "aoc2018/input5.txt" io/resource slurp))

(defn pair-alphabet?
  "주어진 두 알파벳이 대문자와 소문자 쌍이라면 true를 리턴합니다.
   소문자와 대문자의 문자캐릭터 차이는 32"
  [char1 char2]
  (when (every? some? [char1 char2])
    (->> (- (int char1) (int char2))
         Math/abs
         (= 32))))

(defn string->search-list
  [input-string]
  (loop [target-list (seq input-string)
         result-list []]

    (let [target-alphabet (first target-list)
          result-top-alphabet (last result-list)]
      (cond
        (empty? target-list)
        result-list

        (pair-alphabet? target-alphabet result-top-alphabet)
        (recur (rest target-list) (pop result-list))

        :else
        (recur (rest target-list) (conj result-list target-alphabet)))
      )
    )
  )

(defn solve
  [input-string]
  (count (string->search-list input-string)))

(comment
  ;11754
  (solve input-string))
