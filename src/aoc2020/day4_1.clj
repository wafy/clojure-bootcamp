(ns aoc2020.day4-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

; 주어진 여권 배치파일에서 유효한 여권의 수를 계산한다.
; 유효한 여권의 필드는 다음과 같다.

;byr (Birth Year)
;iyr (Issue Year)
;eyr (Expiration Year)
;hgt (Height)
;hcl (Hair Color)
;ecl (Eye Color)
;pid (Passport ID)
;cid (Country ID)

; cid는 필수 필드가 아니라 유효성에서 제외
; 총 7개의 필드 가 모두 있으면 valid 하고 그렇치 않으면 inValid
; valid한 여권의 갯수를 반환하라

(def input (-> "aoc2020/input4.txt"
               io/resource
               slurp
               (str/split #"\n\n"))) ; 각 여권별 구분은 개행 두개로 구분했기 때문에

(defn str->pairs
  [item]
  (->> (str/split item #"[ \n]")
        (map #(str/split % #":"))  ; :으로 key:value 로 나눈다
        (map (fn [[a b]] [(keyword  "day4" a) b]))))

(comment
  (str->pairs (first input))

  ;comp를 사용하여 두함수를 합성해서 map을 한번만 사용해보도록 하자.
(defn map-passport
  [input]
  (->> input
       (map str->pairs)
       (map #(into {} %))                                 ; {키워드: "value"} 의 맵으로 변환한다.
       (map #(dissoc % :day4/cid)))))                     ;cid는 제외한 맵을 새롭게 반환한다.

;(defn f1
;  [a]
;  (+ a 2))
;
;(defn f2
;  [b]
;  (+ b 3))
;
;(def f1f1  (comp f2 f1))
;(f1f1 10)


(comment
  (map-passport input))


(def field-string ["byr" "iyr" "eyr" "hcl" "hgt" "ecl" "pid"])
(def fields (set (map #(keyword "day4" %) field-string)))

(comment
  "#{:day4/eyr :day4/byr :day4/iyr :day4/hcl :day4/ecl :day4/hgt :day4/pid}
  의 형식으로 key만 갖고 있는 맵형태"
  fields input)

(defn solve1
  [input]
  (->> input
       map-passport
       (filter #(= (set (keys %)) fields))
       count))

(comment
  ;196
  (solve1 input))






