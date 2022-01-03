(ns aoc2018.syntext)

(def animals [:mouse :duck :dodo])

(#(str %) :mouse)
(map #(str %) animals)

(reduce + [1 2 3 4])