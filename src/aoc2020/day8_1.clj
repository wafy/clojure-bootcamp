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
       ;         op     number
       (re-seq #"(\S+) +([\+\-]\d+)")
       (map-indexed (fn [index [_ operation arg-number]]
                      (let [op (keyword operation)
                            number (Integer/parseInt arg-number)]
                        {:id         index
                         :op         op
                         :number number})))))

(strings->codes sample-input-strings)

(defn execute-code
  [context]
  (let [{:keys [operations
                accumulator
                pointer
                executed
                execute-log]} context

        execute-command (nth operations pointer)

        {id     :id
         op     :op
         number :number} execute-command

        execute-function (get operation-functions op)

        {result-jump         :jump
         renewal-accumulator :accumulator} (execute-function number accumulator)

        renewal-program-count (+ pointer result-jump)]
    {
     :operations  operations
     :executed    (conj executed id)
     :execute-log (conj execute-log execute-command)
     :accumulator renewal-accumulator
     :pointer     renewal-program-count}))


(defn error?
  [{next-pointer :pointer
    operations   :operations
    executed     :executed}]
  (cond
    (>= next-pointer (count operations))
    (prn "finish")
    (contains? executed (:id (nth operations next-pointer)))
    (prn "error")

    :else
    true))

(defn solve1
  [input-string]
  (let [context {
                 :operations  (strings->codes input-string)
                 :accumulator 0
                 :pointer     0
                 :executed    #{}
                 :execute-log []}]
    (last 
      (take-while error?
                  (iterate execute-code context)))))

(comment
  ;
  ;1217
  (solve1 input-strings))



