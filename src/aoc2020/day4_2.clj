(ns aoc2020.day4-2
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;각필드에 대한 아래 사항에 따라 유효성 검증을 해야한다.
;byr (Birth Year) - four digits; at least 1920 and at most 2002.   --> 1920 >= byr <= 2002
;iyr (Issue Year) - four digits; at least 2010 and at most 2020.  --> 2010 >= iyr <= 2020
;eyr (Expiration Year) - four digits; at least 2020 and at most 2030. --> 2020 >= eyr <= 2030
;hgt (Height) - a number followed by either cm or in:  --> 숫자다음에 cm 또는 in 가 와야한다.
;If cm, the number must be at least 150 and at most 193. --> cm 일 경우 150 >= hgt <= 193
;If in, the number must be at least 59 and at most 76.  --> in 일 경우  59 >= hgt <= 76
;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f. --> #으로 시작하는 6개숫자와 문자 조합
;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth. --> 다음 7개중 하나 amb blu brn gry grn hzl oth
;pid (Passport ID) - a nine-digit number, including leading zeroes. --> 0을 포함한 9자리 숫자
;cid (Country ID) - ignored, missing or not. --> 체크하지 않음

;byr iyr eyr hgt hcl ecl pid
; 위 7가지 유효성을 검증해서 valid한 여권의 갯수를 구하시오

(def input-file (-> "aoc2020/input4.txt" io/resource slurp))
(def input-string (str/split input-file #"\n\n+"))

(defn string->passport-map [string]
  (->> string
       (re-seq #"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)")
       (reduce (fn [m [_ k v]]
                 (assoc  m
                   (keyword k)
                   (cond
                     (#{"iyr" "byr" "eyr" "cid"} k) (Integer/parseInt v)
                     :else v)))
               {})))

(defn hgt-test
  "cm 일 경우 150 >= hgt <= 193
   in 일 경우  59 >= hgt <= 76
  "
  [string]
  (let [[_ height unit] (re-find  #"^(\d+)(in|cm)?$" string)
        num (Integer/parseInt height)]
    (cond (= unit "cm") (and (>= num 150) (<= num 193))
          (= unit "in") (and (>= num 59) (<= num 76)))))

(s/def :passport/byr (s/int-in 1920 2003))
(s/def :passport/iyr (s/int-in 2010 2021))
(s/def :passport/eyr (s/int-in 2020 2031))
(s/def :passport/hgt (s/and hgt-test))
(s/def :passport/hcl (s/and #(re-find #"^#[0-9a-f]{6}" %)))
(s/def :passport/ecl (s/and #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))
(s/def :passport/pid (s/and #(re-find #"^\d{9}$" %)))
(s/def :passport/cid (constantly true))


 (comment
   "byr 2000년이 포함된다면 true"
   (s/valid? :passport/byr 1975)
   "hgt 193cm true"
   (s/valid? :passport/hgt "193cm")
   "hgt 50in true"
   (s/valid? :passport/hgt "60in")
   "hcl #341e13 true"
   (s/valid? :passport/hcl "#6b5442")
   "ecl hzl true"
   (s/valid? :passport/ecl "hzl")
   "pid 637485594 true"
   (s/valid? :passport/pid "637485594"))

(s/def ::passport (s/keys
                    :req-un [:passport/byr :passport/iyr :passport/hgt :passport/hcl
                             :passport/ecl :passport/eyr :passport/pid]
                    :opt-un [:passport/cid]))


(defn solve2
  [input]
  (->> input
       (map string->passport-map)
        (filter #(s/valid? ::passport %))
        count))

(comment
  ;114
  (solve2 input-string))
