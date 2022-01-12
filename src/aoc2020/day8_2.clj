(ns aoc2020.day8-2
  (:require  [clojure.java.io :as io]
             [aoc2020.day8-1 :refer :all]))

(comment
  "입력 명령들 중 딱 하나의 명령이 잘못된 명령입니다.
  그 명령은 jmp 또는 nop 이며, jmp를 nop로 수정하거나 nop를 jmp로 수정하면 프로그램이 끝까지 실행될 수 있습니다.
  명령 하나를 수정해서 프로그램이 끝까지 실행되었을 시점의 누산기의 값이 정답입니다.

  0 nop +0  | 1
  1 acc +1  | 2
  2 jmp +4  | 3
  3 acc +3  |
  4 jmp -3  |
  5 acc -99 |
  6 acc +1  | 4
  7 nop -4  | 5
  8 acc +6  | 6
")

(def sample-input-strings2 (file->string "aoc2020/sample-input8-2.txt"))

(defn swaper [swap-dictionary]
  "swap 매핑 데이터를 받아, 명령 리스트에서 특정 명령의 op code를 스왑해주는 함수를 생성해 리턴합니다.
  생성된 함수는
  명령 리스트를 입력받아, 하나의 op code를 변경합니다."
  (fn [operation-id operation-list]
    (let [
          head (take operation-id operation-list)
          target (nth operation-list operation-id)
          target-op (:op target)
          to-op (get swap-dictionary target-op)
          result (merge target {:op to-op})
          tail (drop (inc operation-id) operation-list)]
      (concat head [result] tail))))

(comment

  ((swaper {:jmp :nop, :nop :jmp}) 1
   [{:id 0, :op :nop, :arg-number 0}
    {:id 1, :op :nop, :arg-number 0}]))

(defn solve2 [input-string]
  (let [operation-list (strings->codes input-string)
        candidate-list (->> operation-list
                            (filter #((:op %) #{:nop :jmp}))
                            (map :id))
        swap (swaper {:nop :jmp, :jmp :nop})]

    (loop [swap-candidates candidate-list]
      (when-not (empty? swap-candidates)
        (let [swap-target (first swap-candidates)
              execute-swap-list (swap swap-target operation-list)
              result (solve1 execute-swap-list)]
          (if (= :error (:result result))
            (recur (rest swap-candidates))
            (merge result)))))))


(comment
  ; 8
  (solve2 sample-input-strings)
  ;501
  (solve2 input-strings))
