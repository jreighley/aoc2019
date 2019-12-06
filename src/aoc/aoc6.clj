(ns aoc.aoc6
  (:require [clojure.string :as s]
            [clojure.set :as cs]))

(def data (->> (slurp "resources/input6")
               (s/split-lines)))

(def orbits
   (for [orbit data]
      [ (subs orbit 0 3 )  (subs orbit 4)]))


(defn find-orbit-body [satellite]
  (first (first (filter #(= satellite (last %) ) orbits))))

(def satellites (set (map last orbits)))
(def roots (set (map first orbits)))
(def bodies (into roots satellites))
(def root "COM")

(defn find-orbit-count
  ([satellite]
   (find-orbit-count satellite  0))
  ([satellite   acc]
   (let [parent (find-orbit-body satellite)
         level (if (nil? parent)
                 acc
                 (inc acc))]
      (if (or (nil? parent) (= parent root))
        level
        (recur parent  level)))))

(reduce + (pmap find-orbit-count  bodies))

;158090

;Part 2

(defn find-ancestors
  ([satellite]
   (find-ancestors satellite []))
  ([satellite parents]
   (let [parent (find-orbit-body satellite)
         newlist  (conj parents parent)]
     (if (nil? parent)
       parents
       (recur parent newlist)))))

(defn solution-2 []
  (let [you-path (find-ancestors "YOU")
        san-path (find-ancestors "SAN")
        short-you-path (remove (set san-path) you-path)
        short-san-path (remove (set you-path) san-path)]
    (count (into short-you-path short-san-path))))

;241


