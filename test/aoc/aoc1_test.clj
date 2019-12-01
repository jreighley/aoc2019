(ns aoc.aoc1-test
  (:require [clojure.test :refer :all])
  (:require [aoc.aoc1 :refer :all]))

(deftest total-fuel-needed-test
  (testing "module calculations"
    (is (= 2 (fuel-needed 12)))
    (is (= 2 (fuel-needed 14)))
    (is (= 654 (fuel-needed 1969)))
    (is (= 33583 (fuel-needed 100756)))))

(deftest recursive-fuel-needed-test
  (testing "fuel + fuelfuel"
    (is (= 2 (recursive-fuel-needed 14)))
    (is (= 966 (recursive-fuel-needed 1969)))
    (is (= 50346 (recursive-fuel-needed 100756)))))



