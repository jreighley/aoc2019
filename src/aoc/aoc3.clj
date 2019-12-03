(ns aoc.aoc3
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def data (map #(s/split % #",")
               (-> (slurp "resources/input3")
                   (s/split  #"\n"))))

(def grid-dirction
  {"D" -
   "U" +
   "R" +
   "L" -})

(def element-to-modify
  {"D" 1 "U" 1 "L" 0 "R" 0})

(defn place-wire  [pointer wire]
  (let [direction (subs wire 0 1)
        distance (Integer/parseInt (subs wire 1))
        target-axis (element-to-modify direction)
        range-start (pointer target-axis)
        range-end ((grid-dirction direction) range-start distance)
        wire-points  (for [co-ordinate (range (min range-start range-end)
                                              (max range-start range-end))]
                        (assoc pointer target-axis co-ordinate))
        end-pointer (assoc pointer target-axis range-end)]
    {:end-point end-pointer
     :points (set wire-points)}))

(defn map-line
  ([line]
   (map-line line #{} [0 0]))
  ([line set pointer]
   (if (empty? line)
       set
       (let [new-wire (place-wire pointer (first line))]
         (recur (rest line) (into set (:points new-wire)) (:end-point new-wire))))))


(defn find-intersections [line1 line2]
  (let [line1-points (map-line line1)
        line2-ooints (map-line line2)
        intersections (cs/intersection line1-points line2-ooints)]
    intersections))

(defn abs [n]
  (max n (* -1 n)))

(defn calc-distance [[x y] [x1 y1]]
  (+  (abs (- x x1)) (abs (- y y1))))

(def prob1-intersections (find-intersections
                           (first data)
                           (nth data 1)))

(def answer-1
  (->> (for [point prob1-intersections]
          [(+ (abs (point 0)) (abs (point 1))) point])
       (sort)
       (first)
       (first)))  ; for my data 4981

(defn follow-wire  [pointer wire-list acc-distance targets]
  (let [wire (first wire-list)
        direction (subs wire 0 1)
        distance (Integer/parseInt (subs wire 1))
        target-axis (element-to-modify direction)
        range-start (pointer target-axis)
        range-end ((grid-dirction direction) range-start distance)
        wire-points  (for [co-ordinate (range (min range-start range-end)
                                              (max range-start range-end))]
                       (assoc pointer target-axis co-ordinate))
        end-pointer (assoc pointer target-axis range-end)
        target-points-crossed (seq (cs/intersection (set wire-points) targets))]
      (if target-points-crossed
        (+ acc-distance (reduce min (map #(calc-distance pointer %) target-points-crossed)))
        (recur end-pointer (rest wire-list) (+ acc-distance distance) targets))))

(def solution2  (->> (for [intersection (reverse (sort (into [] prob1-intersections)))]
                       (let [alpha  (follow-wire [0 0] (nth data 0) 0 (into #{} [ intersection]))
                             beta (follow-wire [0 0] (nth data 1) 0 (into #{} [ intersection]))]
                          {:total-distance-alpha alpha
                            :total-distance-beta  beta
                            :total (+ alpha beta)
                            :intersection intersection}))
                     (map :total)
                     (reduce min)))
;; 164012 for my dataset







