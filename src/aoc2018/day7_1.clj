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

(defn start [graph]
  "출발 경로를 반환하는 함수 "
  (->> graph
       (filter (fn [[_ v]] (-> v :in empty?)))
       (into (sorted-map))))

(comment
  " {\"C\" {:out #{\"A\" \"F\"}}}"
  (start (steps sample-input-file)))


(defn traverse1
  "
 graph : 전체 경로가 담긴 맵
 진행해야할_경로정보: 이번턴에 처리해야할 경로
 찾은_경로: 순서가 판명된 경로 리스트
 "
  ([graph] (traverse1 graph (start graph) []))              ; 처음 호출될때는 파라미터가 1개 joyof p42(destructuring)
  ([graph 진행해야할_경로정보 찾은_경로]                               ; 두번째부터 호출될때 파리머터는 3개
   (prn "-------------------------------------")
   (prn "진행해야할_경로정보: " 진행해야할_경로정보)
   (prn 찾은_경로 찾은_경로)
   (if (empty? 진행해야할_경로정보)                                ; 처리해야할 경로가 없다면 route 반환
     찾은_경로
     (let [진행할_대상 (-> 진행해야할_경로정보 first key)            ; work-context 첫번째값은 이번턴에서 수행해야할 값
           후보_목록 (conj (into #{} 찾은_경로) 진행할_대상)
           다음_진행할_목록  (mapcat (comp :out val) 진행해야할_경로정보)        ;{"C" {:out #{"A" "F"}}} 에서 out을 추출
           다음_진행할_정보 (->> 다음_진행할_목록
                          (filter (fn [k]
                                (empty? (set/difference (get-in graph [k :in]) 후보_목록)) ;전체목록과 후보목록을 뺀 나머지
                                ))
                          (select-keys graph)
                          (into (sorted-map)))]                 ; 알파벳순 정렬을 위해

       (recur graph
              (-> (merge 진행해야할_경로정보 다음_진행할_정보) (dissoc 진행할_대상))
              (conj 찾은_경로 진행할_대상))))))                   ;

(defn solve1 [input-string]
  (apply str (traverse1 (steps input-string))))

(comment
  ;CABDFE
  (solve1 sample-input-file)

  ;SCLPVWAMQYNUHODTRGKBJEFXZI
  (solve1 input-file))

