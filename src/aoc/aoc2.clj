(ns aoc.aoc2
  (:require [clojure.string :as s]))

;; read data file
(def data (doall (mapv #(Integer/parseInt %)(-> (slurp "resources/input2")
                                              (s/split #"\n")
                                              (first)
                                              (s/split #",")))))
;;fn to do return nothing if op end code
(defn op-end [_ _]
  nil)

;;translate ints to apporpiate fns
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
;; not sure this is safe -- but it works for my data.
;; But if the 99 is mod 4 and last I suspect trouble

(def repaired-data
  (-> data
    (assoc 1 12)
    (assoc 2 2)))

(def answer1 (first (process-ops repaired-data)))

(def nouns (range 0 100))
(def verbs (range 0 100))

(defn insert-noun-verb [noun verb]
  (-> data
      (assoc 1 noun)
      (assoc 2 verb)))

(defn all-anwers
  "calculates and maps responses for every noun verb combination"
  []
  (let [iterations (for [noun nouns
                         verb verbs]
                     [noun verb])
        results (for [[noun verb] iterations]
                  {:noun noun
                   :verb verb
                   :result (->> (insert-noun-verb noun verb)
                                (process-ops)
                                (first))})]
    results))

(defn simplify-answer [correct-result]
  (+ (* 100 (:noun correct-result)) (:verb correct-result)))

(defn  reverse-engineer
  "filter all possible responses to the onewe care about"
  [result]
  (first (filter #(= result (:result %)) (all-anwers))))

(def answer2
  (->> 19690720
    (reverse-engineer)
    (simplify-answer)))



