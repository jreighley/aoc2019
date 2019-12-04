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

(def solution-1 (->> data
                     (map digit-seq)
                     (filter increasing?)
                     (filter consecutive-digits?)
                     (count)))  ;1154

(defn check-separate [digit-seq]
   (let [conseq2  (match? (take 2 digit-seq))
         conseq3 (match? (take 3 digit-seq))]
     (when (and conseq2 (not conseq3)) true)))


(defn separated-pairs? [digit-seq]
  (let [front3+ (check-separate (take 3 digit-seq))
        front3- (check-separate (reverse (take 3 digit-seq)))
        back3+ (check-separate (drop 3 digit-seq))
        back3- (check-separate (reverse (drop 3 digit-seq)))
        separated-pair? (or front3+
                            front3-
                            back3+
                            back3-)]
    separated-pair?))


(->> data
     (map digit-seq)
     (filter increasing?)
     (filter consecutive-digits?)
     (filter separated-pairs?))
  ;(count))




;;489 too low

;955 too high













