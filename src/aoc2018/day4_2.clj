(ns aoc2018.day4-2-1
  (:require [clojure.string :as str]
           [aoc2018.day4-1 :refer :all]))

(comment
  "파트 2
  주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.")

(def sample-input-string
  ["[1518-11-01 00:00] Guard #10 begins shift"
   "[1518-11-01 00:05] falls asleep"
   "[1518-11-01 00:25] wakes up"
   "[1518-11-01 00:30] falls asleep"
   "[1518-11-01 00:55] wakes up"
   "[1518-11-01 23:58] Guard #99 begins shift"
   "[1518-11-02 00:40] falls asleep"
   "[1518-11-02 00:50] wakes up"
   "[1518-11-03 00:05] Guard #10 begins shift"
   "[1518-11-03 00:24] falls asleep"
   "[1518-11-03 00:29] wakes up"
   "[1518-11-04 00:02] Guard #99 begins shift"
   "[1518-11-04 00:36] falls asleep"
   "[1518-11-04 00:46] wakes up"
   "[1518-11-05 00:03] Guard #99 begins shift"
   "[1518-11-05 00:45] falls asleep"
   "[1518-11-05 00:55] wakes up"])


(defn to-sleep-datum
  "한 번 잠들었다 일어난 기록의 시간을 hash-map에 담아 리턴합니다.
  시간은 분(minute)만 기록합니다.
  예) [1518-11-21 00:30] falls asleep [1518-11-21 00:38] wakes up
     => { :sleep 30, :wake 38, :sleeping-time 8 } ; 30분에 잠들어서, 38분에 깨어났으므로, 8분간 잠을 잤다는 내용.
  "
  [a-sleep-log]
  (let [
        [_ sleep-str wake-str] (re-find #"(\d+)\]\s*falls asleep\s*\[.+?(\d+)\]\s*wakes up" a-sleep-log)
        sleep (Integer/parseInt sleep-str)
        wake (Integer/parseInt wake-str)]
    {:sleep         sleep
     :wake          wake
     :sleeping-time (- wake sleep)}))


(defn to-day-log
  "한 줄로 이루어진 하루의 로그를 분석해 map으로 생성해 리턴합니다."
  [log-string]
  (let [
        [_ guard-id date] (re-find #"^[^#]*?#(\d+)\s*\[(\d+-\d+-\d+)" log-string)
        removed-guard-id (str/replace log-string #"^#\d+\s*" "")
        split-logs (str/split-lines
                     (str/replace removed-guard-id #"(wakes up)\s*" "$1\n"))
        sleep-data (map to-sleep-datum split-logs)]
    (map
      #(conj {:guard-id (Integer/parseInt guard-id) :date date} %)
      sleep-data)))

(comment
  (to-day-log (str "[1518-11-01 00:00] Guard #10 "
                 "[1518-11-01 00:05] falls asleep [1518-11-01 00:25] wakes up "
                 "[1518-11-01 00:30] falls asleep [1518-11-01 00:55] wakes up ")))


(defn simplify-logs
  "입력된 raw log를 날짜-시간 기준으로 정렬하고, 여러줄로 나뉘었던 로그를 하루 기준으로 분류한 리스트로 리턴합니다."
  [input-strings]
  (let [log-one-row (str/join " " (sort input-strings)) ;날짜별로 정렬
        split-logs (str/split-lines
                     (str/replace log-one-row
                                  #"\[[^]]+\] Guard (#\d+) begins shift" "\n$1"))]
  ;  (println split-logs)
    (reduce into
            (map to-day-log
                 (filter
                   #(not (re-matches #"^(#\d+)?\s*" %)) split-logs)))
    ))

(comment
  (simplify-logs sample-input-string))

(defn most-of
  "frequencies 함수로 빈도를 계산한 결과를 받아 그 중 가장 높은 빈도를 가진 엔트리를 리턴합니다."
  [list]
  (let [[value frequency] (first (sort-by val > (frequencies list)))]
    {:value value, :frequency frequency}))

(comment
  (most-of [:a :a :b :b :b :c]))

(defn solve2
  [input-strings]
  (
    let [
         날짜별-로그-리스트 (simplify-logs input-strings)
         날짜별-수면-기록 (map #(assoc % :minutes (range (:sleep %) (:wake %))) 날짜별-로그-리스트)
         경비원별-수면-기록 (group-by :guard-id ; 가드아이디별 그롭화
                              (map #(select-keys % [:guard-id :minutes]) 날짜별-수면-기록)) ;가드별 수면 시간을 찾는다.
         경비원별-가장-많이-잠들었던-분 (map (fn [[guard-id v]]
                                  (merge
                                    {:guard-id guard-id}
                                    (most-of (reduce into (map #(:minutes %) v)))))
                                경비원별-수면-기록)
         정답-대상자 (last (sort-by :frequency 경비원별-가장-많이-잠들었던-분))]

    ;날짜별-수면-기록
    ;경비원별-수면-기록
    ;경비원별-가장-많이-잠들었던-분
    ;정답-대상자
    (merge 정답-대상자 {:solution (* (:guard-id 정답-대상자) (:value 정답-대상자))}) ;결과를 map {} 으로 반환

    )

  )

(comment
  (solve2 input-strings))