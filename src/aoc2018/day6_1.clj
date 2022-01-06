(ns aoc2018.day6-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input-file (-> "aoc2018/input6.txt" (io/resource) (slurp)))
(def input-strings (str/split-lines input-file))


(defn parse-data
  "[x y] 로 반환 합니다."
  [input-strings]
  (->> (for [line input-strings]
       (str/split line #", "))
       (map #(vector (Integer/parseInt (first %)) (Integer/parseInt (second %))))))

(comment
  (parse-data input-strings))

(defn manhattan-distance
  "두 좌표를 입력 받아 맨하탄 거리를 계산하여 반환 합니다."
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(comment
  (manhattan-distance [1, 0] [1, 1]) ;1
  (manhattan-distance [0, 0] [-1, -1]) ;2
  (manhattan-distance [-1 -1] [1 1])) ;4

(defn area-limits
  "좌표의 유한간 값을 얻기 위한 최소 최대값을 구합니다. [1 2 3 4]"
  [xys]
  [(apply min (map first xys))
   (apply max (map first xys))
   (apply min (map second xys))
   (apply max (map second xys))])

(comment
  (area-limits (parse-data input-strings))

(defn closest-coordinates
  "(tick)동일한 거리에 여러개가 있는 경우 가장 가까운 x,y 또는 nil에 가장 가까운 좌표를 반환합니다."
  [xys x y]
  (let [dists (sort-by first < (for [xy xys]
                                 [(manhattan-distance [x y] xy) xy]))]
    (when-not (= (ffirst dists)
                 (first (second dists)))
      (second (first dists)))))

(comment
  (closest-coordinates (parse-data input-strings)   2)))

(defn calculate-area
  "해당 영역에서 가장 가까운 x y 좌표를 반환합니다."
  [xys [x1 x2 y1 y2]]
  (for [y (range y1 (inc y2))
        x (range x1 (inc x2))]
    (closest-coordinates xys x y)))


(defn infinits
  "무한 영역에 있는 x y 좌표를 반환합니다."
  [area [x1 x2 y1 y2]]
  (let [x (inc (- x2 x1))
        y (inc (- y2 y1))
        down (for [n (range 0 x)] (nth area n))
        up (for [n (range (- (* y x) x) (* y x))] (nth area n))
        left (for [n (range 0 (* y x) x)] (nth area n))
        right (for [n (range (dec x) (* y x) x)] (nth area n))]
    (distinct (filter some? (concat down up left right)))))

(comment
  (area-limits (parse-data input-strings)))

; (->> ) 부분을 별도의 함수로 분리하여 추상화의 레밸을 맞춰야한다.
; 문제를 풀때 어떻게 쪼개서 해결해야 하는지
; 한글로 단계를 적어서 풀어서 함수로 분리
; 의식적으로 하고자 해야한다.
; solve 함수내에서 결과에 필요한 부분을 한글로 적어서 단계별로 분리해보자.
; aco2020 4번 문제를 풀고 풀기전에 노선 링크를 보자.
  ; 1.blog를 보라
  ; 2. 문제를 풀어라
  ; 3. 스팩을 써라 (처음부터 스팩을 써도 된다.) 참고링크 : https://clojure.org/guides/spec , https://clojure.org/about/spec
; sort last last 대신 min-key max-key 를 적용해서 변경이 필요하다.
; 1 가장 큰값을 구한다
; 2 정렬해서 가장 큰값을 구한다. (구현으로써 코드가 이해되는 의미)
(defn solve1 [xys]
  (let [limits (area-limits xys) ;주어진 좌표에서 최소 최대 좌표를 반환한다. [44 313 44 342]
        area (calculate-area xys limits) ; 최대 영역 안에서 모든 좌표들을 반환한다.
        finite (set/difference (into #{} xys) (into #{} (infinits area limits)))] ;주어진 좌표에서 무한을 뺀것이 유한한 좌

    (->> area
         (filter #(finite %))
         frequencies
         (sort-by val)
         last
         last)))

(comment
  ;2917
  (solve1 (parse-data input-strings)))