(ns aoc.aoc8)

(def data (->> (slurp "resources/input8")
               (partition 25)
               (map vec)
               (partition 6)
               (map vec)))

(defn layer-dist [layer]
  (let [all-pixels (flatten layer)
        digit-counts (frequencies all-pixels)]
    digit-counts))

(defn solution-1 []
  (let [layertats (map layer-dist data)
        layer-calcs (for [layer layertats]
                      {:zeros (get layer \0)
                       :calc (* (get layer \1)
                                (get layer \2))})]
    (first (sort-by :zeros layer-calcs))))  ; 1320

(def transparent? #(= \2 %))

(defn merge-pixel [layer-seq]
  (->> layer-seq
       (remove transparent?)
       (first)))

(defn color-ize [px]
    (if (or (= \1 px) (nil? px))
      "█"
      " "))

(defn get-pixel [data row col]
  (let [rowlist (map #(get % row) data)
        col-list (map #(get % col) rowlist)]
    (color-ize (merge-pixel col-list))))

(defn solution-2 []
  (for [row (range 6)]
    (apply str (for [col (range 25)]
                 (get-pixel data row col)))))


;( "███   ██  █   ██  █ ███  "
;  "█  █ █  █ █   ██ █  █  █ "
;  "█  █ █     █ █ ██   █  █ "
;  "███  █      █  █ █  ███  "
;  "█ █  █  █   █  █ █  █ █  "
;  "█  █  ██    █  █  █ █  █ ")


;RCKYR