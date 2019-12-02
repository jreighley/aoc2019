(ns aoc.aoc2-test
  (:require [clojure.test :refer :all])
  (:require [aoc.aoc2 :refer :all]))

(deftest process-ops-test
  (testing "ops-codes"
    (is (= [2 0 0 0 99]
           (process-ops  [1,0,0,0,99])))
    (is (= [2,3,0,6,99])
        (process-ops  [2,3,0,3,99]))
    (is (= [2,4,4,5,99,9801])
        (process-ops  [2,4,4,5,99,0]))
    (is (= [30,1,1,4,2,5,6,0,99])
        (process-ops  [1,1,1,4,99,5,6,0,99]))))


;1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
;2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
;2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
;1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.