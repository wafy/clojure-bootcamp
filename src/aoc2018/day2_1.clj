(ns aoc2018.problem-2-1
  (:require [clojure.string :as str]
    [clojure.java.io :as io]))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12


(def input-file (-> "aoc2018/input2.txt" (io/resource) (slurp)))
(def input-strings (str/split-lines input-file))

;;frequencies https://clojuredocs.org/clojure.core/frequencies
;;문자열의 문자와 수를 계산하는 함수
(map frequencies input-strings)

;; 문자를 제거하고 수만 반환한다.
;(class (map vals (map frequencies input-strings)))
(map vals (map frequencies input-strings))

;;숫자에서 중복을 제거한다.
(map distinct (map vals (map frequencies input-strings)))

;;코드를 정리
;; ->> 매크로이며(?) 여러가지 함수를 호출하여 사용할때 자바의 스트림처럼 연속으로 처리할 수 있어 보인다.
(->> (map frequencies input-strings) ;; 주어진 문자열의 문자와 수를 반환
     (map vals) ;; 문자를 제거
     (map distinct)) ;;중복 제거


;; 2, 3번 나온 문자열만 반환해보자.
;;filter를 익힌다. 인수로 진위 함수(true?)와 컬렉션을 받는다.
;;필터 처리를 함수로 추출한다.
(defn only-two-three [num]
  (filter #(or (= 2 %1) (= 3 %1)) num))

(->> (map frequencies input-strings)
     (map vals)
     (map distinct )
     (map only-two-three))

(->  frequencies
     (map input-strings)
     (map vals)
     (map distinct)
     (map only-two-three))


;;flatten 을 익힌다.
;;시퀀스에서 중복을 제거한 후 반환하다.
(->> (map frequencies input-strings)
     (map vals)
     (map distinct)
     (map only-two-three)
     (flatten))

(->> (map frequencies input-strings)
     (map vals)
     (map distinct)
     (map only-two-three)
     (flatten)
     (frequencies)
     (vals)
     (apply *))


(defn solve-2-1 [strings]
  (->> strings
       (map frequencies)
       (map vals)
       (map distinct)
       (map only-two-three)
       (flatten)
       (frequencies)
       (vals)
       (apply *))
     )

(solve-2-1 input-strings)
