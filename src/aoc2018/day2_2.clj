(ns aoc2018.problem-2-2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

;; 해밍거리 알고리즘을 응용한 문제로 파악  해밍거리 알고리즘이란: 길이가 같은 두 문자열 사이의

(def input-file (-> "aoc2018/input2.txt" (io/resource) (slurp)))
(def input-strings (str/split-lines input-file))


;; for는 컬랙션내 본문에서 처리하여 조건에 따른 새로운 리스트를 반환한다.
;; :let 수정자 for 구문에서 let을 바인딩
;; :when 수정자 진위함수가 참일때 실행
;; get 키값에 매핑된 값을 가져온다.
(defn collect-intersection
  "두 리스트를 비교하여, 위치와 값이 같은 아이템을 리스트로 수집해 리턴합니다
   주어진 두 단어 사이에 중복되는 단어를 가져오기 위함입니다.
  "
  [list1 list2]
  (for [index (range (count list1))
        :let [char1 (get list1 index),
              char2 (get list2 index)]
        :when (= char1 char2)] ;; 두 문자열이 같다면
    char1)
  )

(defn analyze-two-string
  " 두 문자열을 받아 분석한 결과를 {} 맵으로 리턴합니다"
  [string1 string2]
  (when-not (= string1 string2)
    ;let 구문안에 :키워드로 할당하면 맵으로 반환
    (let [intersection (collect-intersection string1 string2)]
      {
       :string1             string1
       :string2             string2
       :mismatch-count      (- (count string1) (count intersection)) ;; 전체단어와 중복단어를 빼면 다른단어의 수
       :intersection-string (str/join  intersection) ;; 문자구분을 구분기호로 변환하여 반환 str/join "," intersection
       }
      )))

(defn solve-2-2-f [string-list]
  (let [
        strings (for [string1 string-list,
                      string2 string-list]
                  [string1 string2])
        ]
    (->> strings
         (map (fn [string1] (analyze-two-string (first string1) (second string1))))   ; [ abcd랑 다른 문자열 1개를 받아서 분석하는 함수, bbcd랑 다른 문자열 1개를 받아서 분석하는 함수 ]
         (filter some?) ;;nil 제거
         (filter #(= 1 (get % :mismatch-count))) ;문제에서 주어진 조건이 하나의 문자가 다른것을 추출하기 위해
         (map #(% :intersection-string)) ; 중복되지 않는 단어를 추출
         (first) ;맨처음 한개만 가져오기위
  )))

;(solve-2-2-f [ "fguij" "fgvij" ])
(solve-2-2-f input-strings)

