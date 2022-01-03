(ns aoc2018.problem-1-2)

(comment
  "
 파트 2
 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
  예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
  0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
  ")

(defn solve1-2
  [numbers last-sum number-set]
  (let [next-sum (+ last-sum (first numbers))]
    (if (number-set next-sum)
      ; return
      next-sum
      (recur
        (rest numbers)
        next-sum
        (conj number-set next-sum)))))


(let [ints (parse "aoc2018/input1.txt")]
  (solve1-2 ints 0 0))