(ns aoc2018.day6-2
  (:require [aoc2018.day6-1 :refer :all]))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 이하인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

; for문을 map -> reduce 단계 별로 구분이 필요하다.
; 단계별로 나뉘어져야한다.
;

(defn total-manhattan-dist
  [init-points point]
  (reduce + (map (fn [init-point]
                   (manhattan-distance init-point point))
                 init-points)))

(defn sum-coordinates
  [init-points [limit-x1 limit-x2 limit-y1 limit-y2]]
  (let [all-points (for [y (range limit-y1 (inc limit-y2))
                         x (range limit-x1 (inc limit-x2))]
                     [x y])]

    (map #((partial total-manhattan-dist init-points) %) all-points)))

(defn safe-area
  [safe-limit sum-of-coordinates]
  (->> sum-of-coordinates
       (filter #(> safe-limit %))
       count))

(defn solve2
  [input safe-limit]
  (->> (area-limits input)
       (sum-coordinates input)
       (safe-area safe-limit)
       ))

(comment
  ;44202
  (solve2 (parse-data input-strings) 10000))
