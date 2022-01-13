(ns aoc2018.day7-1
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]))

(comment
  "
  Step C must be finished before step A can begin.
  Step C must be finished before step F can begin.
  Step A must be finished before step B can begin.
  Step A must be finished before step D can begin.
  Step B must be finished before step E can begin.
  Step D must be finished before step E can begin.
  Step F must be finished before step E can begin.

  이 입력은 다음과 같은 뜻을 갖는다.
  C 를 마쳐야 A를 할 수 있다.
  C 를 마쳐야 F를 할 수 있다.

  아래는 이 입력의 작업 의존도를 표현하는 그림이다.
      -->A--->B--
    /    \\      \\
    C     -->D----->E
     \\          /
      ---->F-----
   C 부터 시작해서 E 까지 작업을 완료하는 단계를 문자열로 표현하면 된다.
   같은 단계에서 할 수 있는 작업이라면 우선순위는 알파벳순으로 따지면 된다.

  ")

(def sample-input-file (-> "aoc2018/sample-input7.txt" (io/resource) (slurp)))
(def sample-input-strings (str/split-lines sample-input-file))
(def input-file (-> "aoc2018/input7.txt" (io/resource) (slurp)))
(def input-strings (str/split-lines input-file))

(defn parse-line [line]
  "조건값만 파싱해 리턴하는 함수 "
  (let [[_ a b] (re-matches #".*\b([A-Z])\b.*\b([A-Z])\b.*" line)]
    [a b]))

(comment
  ;["O" "T"]
  (parse-line "Step O must be finished before step T can begin"))

(defn steps [input]
  "A {:in #{\"C\"}, :out #{\"B\" \"D\"}}
   X {in:x를 통해 들어오는 값(즉 이전 종료 값)  out:x 수행하고 나갈 값(이후 종료해야할 값)}"
  (reduce (fn [graph [a b]]
            (-> graph
                (update-in [a :out] (fnil conj (sorted-set)) b)
                (update-in [b :in] (fnil conj (sorted-set)) a)))
          (sorted-map)                                      ;알파벳순으로 정렬해야함 맵으로 초기화
          (->> input
               str/split-lines
               (map parse-line))))

(comment
  "A {:out #{\"B\" \"D\" \"G\" \"I\"}, :in #{\"C\" \"P\"}},"
  (steps sample-input-file))

(defn candidate-node [graph]
  (->> graph
       (filter (fn [[_ v]] (-> v :in empty?)))
       (into (sorted-map))))

(comment
  " {\"C\" {:out #{\"A\" \"F\"}}}"
  (candidate-node (steps sample-input-file)))

(defn traverse
  "
 graph : 전체 경로가 담긴 맵
 진행해야할_경로정보: 이번턴에 처리해야할 경로
 찾은_경로: 순서가 판명된 경로 리스트
 "
  ([graph] (traverse graph (candidate-node graph) []))
  ([graph proceed-node discovered-node]
   (if (empty? proceed-node)
     discovered-node
     (let [target-node (-> proceed-node first key)
           find-node-list (conj (into #{} discovered-node) target-node)
           candidate-node (mapcat (comp :out val) proceed-node)
           candidate-node-list (->> candidate-node
                                    (filter (fn [k] (empty? (set/difference (get-in graph [k :in]) find-node-list))))
                                    (select-keys graph)
                                    (into (sorted-map)))]

       (recur graph
              (-> (merge proceed-node candidate-node-list) (dissoc target-node))
              (conj discovered-node target-node))))))

(defn solve1 [input-string]

  (apply str (traverse (steps input-string))))

(comment
  ;CABDFE
  (solve1 sample-input-file)

  ;SCLPVWAMQYNUHODTRGKBJEFXZI
  (solve1 input-file))

