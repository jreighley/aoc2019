(ns aoc.aoc1
  (:require [clojure.string :as s]
            [clojure.edn :as edn]))

(def data (->> (slurp "resources/input1")
               (s/split-lines)
               (map #(Integer/parseInt %))))

(defn fuel-needed [ mass]
  (-> mass  ;take the mass
    (/ 3)   ;devide it by 3
    (int)   ;keep the int part
    (- 2)))  ;subract 2

(defn total-fuel-needed [modules]
  (->> modules                 ;take the list of modules
    (map #(fuel-needed %))    ;map the fuel needed function over it
    (reduce +)))               ;add them all together

(def answer1 (total-fuel-needed data))  ; 3464458 for my dataset

(defn recursive-fuel-needed
  ([mass]
   (recursive-fuel-needed 0 mass))  ; if called without a accumulator start at 0
  ([accfuel mass]
   (let [added-fuel (fuel-needed mass)]
     (if (not (pos? added-fuel))
       accfuel       ; if additional fuel <1 return out accumulated amount
       (recur (+ accfuel added-fuel) added-fuel)))))   ;; else recur with accumulated fuel and calculate fuel for added fuel

(defn total-fuel-needed-recursive [modules]
  (->> modules
       (map #(recursive-fuel-needed %))
       (reduce +)))

(def answer2 (total-fuel-needed-recursive data))  ;5193796 for my dataset