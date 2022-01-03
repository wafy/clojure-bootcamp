(ns aoc2018.day4-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;당신은 연꾸실에 침입할려고함.
;연구실 밖에 경비원들이 있음.
;다른사람도 침입할려고한 흔적이 있음.
;몇달동안 경비초소를 관찰하면서 경비 순찰 로그를 기록
;경비원들이 자고 일어나는 기록이 있음
;가장많이 잔 경비원을 찾아내고 경비원의 잠을 잔 공통 분을 찾아 내라.

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 2시 5분~11분, 3시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(def input-file (-> "aoc2018/input4.txt" (io/resource) (slurp)))
(def input-strings (str/split-lines input-file))

; log parsing
;input-strings

;입력받은 로그를 날짜순으로 정렬하여 한줄로 반환하는 함수
(defn log->one-row [input-strings]
  (->> input-strings
       sort ;날짜순으로 자동 정렬
       (map #(str/replace % #"^\[.+?(\d\d)\]" " $1"))  ;[xxxx-xx-xx  min]    min (숫자두개를 가져오기)
       str/join ;한줄로 만든다. -> 가드별 잠잔 시간을 구분하기 위해
       ))

;경비원 한명의 로그를 맵으로 변환하는 함수
(defn log->guard-sleep-data [guard-log]
  "경비원 한명의 로그를 맵으로 변환 :guard-id :sleep-wake-list {(:sleep 잠든시간 :wake 일어난 시간 :during 잠잔시간)}
  param :  #331 begins shift 19 falls asleep 44 wakes up 52 falls asleep 56 wakes up
  "
  (let [
        guard-id (str/replace guard-log #"^\s*#(\d+).*$" "$1") ; guard-id 추출
        sleep-wake-list (->> guard-log
                             (re-seq #"(\d\d) falls asleep (\d\d) wakes up") ;패턴에 일치하는 값을 반환(잠든시간 일어난시간)
                             (map rest) ; 첫번째 정보는 제거하고 시간만 표시하기 위함.(근무시간은 제거)
                             (map (fn [[잠든시간 일어난시간]]
                                    (let [
                                          sleep (Integer/parseInt 잠든시간),
                                          wake (Integer/parseInt 일어난시간),
                                          ]

                                      {:sleep  sleep
                                       :wake   wake
                                       :during (- wake sleep) ; 일어난 시간 - 잠들기 시작 = 잠든 시간
                                       })))
                             )
        ]
    (when-not (re-find #"^\s*$" guard-id)
      {
       :guard-id        (Integer/parseInt guard-id)
       :sleep-wake-list sleep-wake-list
       })
    )
  )

(defn solve1
  [input-strings]
  (let [
        log-on-row (log->one-row input-strings)
        guard별-로그-list (str/split-lines (str/replace log-on-row #"\d\d Guard" "\n")) ;교대근무시간(00) Guard를 제거
        guard-data-list (map log->guard-sleep-data guard별-로그-list )
        sleep-during-list (map (fn [{guard-id :guard-id, sleep-data :sleep-wake-list}] ;가드아이디별 잠든 시간 계산
                                 {:guard-id   guard-id
                                  :during-sum (->> sleep-data
                                                   (map :during)
                                                   (apply +))
                                  }
                                 ) guard-data-list)
        guard-sleep-group (group-by :guard-id sleep-during-list) ; 가드아이디별 group 화
        sleep-sum (map (fn [[key value-list]] ;가드아이디별 총 잠잔시간
                         {
                          :guard-id    key
                          :total-sleep (->> value-list
                                            (map :during-sum)
                                            (apply +))
                          }
                         )
                       guard-sleep-group
                       )
        best-sleep-guard (last (sort-by :total-sleep < sleep-sum)) ; 가장많이 잔 가드 추출
        frequent-minute (->> guard-data-list
                             (filter #(= (:guard-id best-sleep-guard)
                                         (:guard-id %)))
                             (map (fn [{sleep-list :sleep-wake-list}]
                                    (->> sleep-list
                                         (map (fn [{sl :sleep wa :wake}]
                                                (range sl wa)
                                                ))
                                         (reduce into [])
                                         )
                                    ))
                             (reduce into []) ;백터로 합쳐 반환
                             frequencies ;요소의 카운트 반환
                             (sort-by val >) ;내림차순 정렬
                             first ;41 14 첫번째 요소
                             first ;41 첫번째 요소
                             )
        ]
    frequent-minute
    {
     :guard-id (:guard-id best-sleep-guard)
     :freq-min frequent-minute
     :solution (* (:guard-id best-sleep-guard) frequent-minute)
     }
    )
  )


(solve1 input-strings)