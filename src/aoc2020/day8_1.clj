(ns aoc2020.day8-1
  (:require [clojure.java.io :as io]))

(comment "
  중복 명령이 실행되기 직전의 acc 값을 출력하세요.
  연산 인수
  acc +1 (증가)
  jmp +1 (점프)
  nop 0  (아무것도 하지 않음)

  연산 인수 | 순서
  ------- |----
  nop +0  | 1
  acc +1  | 2, 8(!)
  jmp +4  | 3
  acc +3  | 6
  jmp -3  | 7
  acc -99 |
  acc +1  | 4
  jmp -4  | 5
  acc +6  |

 위 예시를 실행하면
 8번째에서 중복실행되었으므로 이전 실행된 값은
 acc에는 값이 누적됨
 =+ 1 3 1 = 5가 정답

 'iterate로 변경
")

(defn file->string [file-address]
  (-> file-address
      io/resource
      slurp))

(def input-strings (file->string "aoc2020/input8.txt"))
(def sample-input-strings (file->string "aoc2020/sample-input8.txt"))

(def operation-functions
  "각 어셈블리 명령을 처리하는 함수를 정의합니다."
  {:nop (fn [_ accumulator]
          {:jump        1
           :accumulator accumulator})

   :acc (fn [number accumulator]
          {:jump        1
           :accumulator (+ accumulator number)})

   :jmp (fn [number accumulator]
          {:jump        number
           :accumulator accumulator})})

(comment operation-functions)

(defn strings->codes
  "주어진 문자열을 코드 리스트로 변환해 리턴합니다."
  [strings]
  (->> strings
       ;         op     arg-number
       (re-seq #"(\S+) +([\+\-]\d+)")
       (map-indexed (fn [index [_ operation arg-number]]
                      (let [op (keyword operation)
                            number (Integer/parseInt arg-number)]
                        {:id         index
                         :op         op
                         :number number})))))

(comment
  " 샘플데이타는 다음과 같이 반환된다.
({:id 0, :op :nop, :number 0}
 {:id 1, :op :acc, :number 1}
 {:id 2, :op :jmp, :number 4}
 {:id 3, :op :acc, :number 3}
 {:id 4, :op :jmp, :number -3}
 {:id 5, :op :acc, :number -99}
 {:id 6, :op :acc, :number 1}
 {:id 7, :op :jmp, :number -4}
 {:id 8, :op :acc, :number 6})
  ")
(strings->codes sample-input-strings)

(defn solve1
  [input]
  (loop [
         operation-list input
         accumulator 0
         program-count 0
         execute-set-list #{}
         execute-log []]
    (let [
          ; index에 해당하는 실행할 명령을 가져온다.
          execute-command (nth operation-list program-count)

          {id :id op :op number :number} execute-command
          ; op 에 해당하는 처리할 함수를 가져온다.
          execute-function (get operation-functions op)

          {result-jump :jump renewal-accumulator :accumulator} (execute-function number accumulator)
          ;
          renewal-program-count (+ program-count result-jump)
          renewal-execute-log (conj execute-log execute-command)]
      (prn "--------------------------------")
      (prn "program-count: " program-count)
      (prn "result-jump: " result-jump)
      (prn "renewal-accumulator: " renewal-accumulator)
      (prn "execute-set-list: " execute-set-list)
      (prn "renewal-program-count: " renewal-program-count)
      (prn "renewal-accumulator: " renewal-accumulator)

      (cond
        (>= renewal-program-count (count operation-list))
        {:result         :finish
         :accumulator    renewal-accumulator}

        (contains? execute-set-list id)
        {:result         :error
         :accumulator    accumulator}

        :else
        (recur operation-list
               renewal-accumulator
               renewal-program-count
               (conj execute-set-list id)
               renewal-execute-log)))))
reduce
(comment
  ; 5
  (solve1 (strings->codes sample-input-strings))
  ;1217
  (solve1 (strings->codes input-strings)))

