(ns aoc2018.day3-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
           [clojure.set :as set]))

(comment "
   파트 1
   다음과 같은 입력이 주어짐.

   #1 @ 1,3: 4x4
   #2 @ 3,1: 4x4
   #3 @ 5,5: 2x2

   # 뒤에 오는숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
   입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.
   id x,y: 격자

        ........
        ...2222.
        ...2222.
        .11XX22.
        .11XX22.
        .111133.
        .111133.
        ........

   여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
   겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
   "
  )

(def input-file (-> "aoc2018/input3.txt" (io/resource) (slurp)))
(def input-strings (str/split-lines input-file))

(defn parse-map [line]
  (let [[_ id & dims] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" line)
        [left top width height] (map #(Integer/parseInt %) dims)]
    {:id id :left left :top top :width width :height height}))

;(parse-map (str "#16 @ 393,939: 19x21"))

(defn claim-map
  "모든 x y의 값을 하나의 맵에 모두 저장합니다."
  [{:keys [left top width height]}]
  (into {} ; 하나의 맵으로 반환
        (for [x (range left (+ left width))
              y (range top (+ top height))]
          [[x y] 1])))

;(claim-map {:id "16" :left 393 :top 939 :width 2 :height 2})

(defn merge-claims [input]
  (->> input
       (map parse-map)
       (map claim-map)
       (reduce (fn [m c] (merge-with + m c)) {})))  ;두개의 맵을 하나의 맵으로 키가 같으면 값을 더해서 반환

;(merge-claims (str "#16 @ 393,939: 19x21"))

(defn solve-1
  [input]
  (->> input
       merge-claims
       vals
       (filter #(> % 1))
       count
       ))

(solve-1 input-strings)


(comment
  )


