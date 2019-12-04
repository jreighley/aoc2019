(ns aoc.aoc4
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def data (range 240920 (inc 789857)))

(def digit-seq #(for [digit  (str %)]
                    (->> digit
                         str
                         (Integer/parseInt))))

(def increasing? #(apply <= %))

(def match? #(apply = %))

(defn consecutive-digits? [digit-seq]
  (->> (into (partition 2 digit-seq) (partition 2 (rest digit-seq)))
       (filter match?)
       (seq)))

(def sol1-filter (->> data
                      (map digit-seq)
                      (filter increasing?)
                      (filter consecutive-digits?)))

(def solution-1 (count sol1-filter))

(defn check-separate [digit-seq]
   (let [conseq2  (match? (take 2 digit-seq))
         conseq3 (match? (take 3 digit-seq))]
     (when (and conseq2 (not conseq3)) true)))

(defn clean-max
  "Remove nils and return maximum count"
  [count-seq]
  (let [count-seq (filter some? count-seq)
        conseq-count (when (seq count-seq)
                       (reduce max count-seq))]
       (if conseq-count
         conseq-count
         1)))

(defn count-matching-front [digit-seq]
  (->> (for [n (range 2 7)]
         (if (match? (take n digit-seq)) n))
       clean-max))

(defn separated-pairs? [digit-seq]
  (when (<= 3 (count digit-seq))
    (let [front-match (count-matching-front digit-seq)
          back-match (count-matching-front (reverse digit-seq))]
      (cond (= 2 front-match) true
            (= 2 back-match) true
            :else (recur (drop front-match digit-seq))))))

(def solution-2 (->> sol1-filter
                     (filter separated-pairs?)
                  (count)))












