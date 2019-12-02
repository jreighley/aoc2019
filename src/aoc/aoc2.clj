(ns aoc.aoc2
  (:require [clojure.string :as s]))

(def data (doall (mapv #(Integer/parseInt %)(-> (slurp "resources/input2")
                                              (s/split #"\n")
                                              (first)
                                              (s/split #",")))))
(defn op-end [_ _]
  nil)
(def commands {1 +
               2 *
               99 op-end})

(defn process-ops
  ([command-seq]
   (process-ops command-seq 0))
  ([command-seq cursor-pos]
   (let [op-seq (subvec command-seq cursor-pos)
         [op input1 input2 output & more] op-seq
         calc-result ((commands op)
                      (command-seq input1)
                      (command-seq input2))]
     (if (= 99 (first more))
       (assoc command-seq output calc-result)
       (recur (assoc command-seq output calc-result) (+ 4 cursor-pos))))))

(def repaired-data
  (-> data
    (assoc 1 12)
    (assoc 2 2)))

(def nouns (range 0 100))
(def verbs (range 0 100))

(defn configure-data [noun verb]
  (-> data
      (assoc 1 noun)
      (assoc 2 verb)))

(defn all-anwers []
  (let [iterations (for [noun nouns
                         verb verbs]
                     [noun verb])
        results (for [[noun verb] iterations]
                  {:noun noun
                   :verb verb
                   :result (->> (configure-data noun verb)
                                (process-ops)
                                (first))})]
    results))

(def correct-result (first (filter #(= 19690720 (:result %)) (all-anwers))))

(def answer2 (+ (* 100 (:noun correct-result)) (:verb correct-result)))

(def answer1 (first (process-ops repaired-data)))



