(ns cljs-d3.gamelogic
  (:require [om.core :as om :include-macros true]
            [clojure.core.reducers :as reducers]))

(defn find-zero-indexes [lst]
  "takes a list of numbers, and returns the indexes of 0 values"
  (keep-indexed (fn [idx v]
                  (when (= 0 v) idx)) lst))

(defn new-tile []
  "10% chance of 4 tile, 90% chance of 2 tile"
  (if (zero? (rand-int 10)) 4 2))

(defn unmemoized-merge-row [row]
  "applies 2048 rules of merging from right to left"
  (let [non-zeroes (remove zero? row)
        merged (loop [lst non-zeroes
                      acc []]
                 (cond 
                  (empty? lst) acc
                  (= (first lst) (second lst)) (recur (drop 2 lst) (conj acc (* 2 (first lst))))
                  :else (recur (rest lst) (conj acc (first lst)))))
        num-zeroes-removed (- 4 (count merged))]
    (into merged (vec (repeat num-zeroes-removed 0)))))

(def merge-row (memoize unmemoized-merge-row))

(defn rotate-grid [grid]
  "rotate a grid 90 degrees clockwise" 
  (when grid
    (into [] (for [idx (list 12 8 4 0 
                             13 9 5 1 
                             14 10 6 2 
                             15 11 7 3)]
               (nth grid idx)))))

;; for other directions, rotate then apply move-left and rotate back
;; if not a valid, return false
(defn unmemoized-move-left [grid] 
  (let [row1 (subvec grid 0 4)
        row2 (subvec grid 4 8)
        row3 (subvec grid 8 12)
        row4 (subvec grid 12 16)
        res (-> (map merge-row (list row1 row2 row3 row4)) 
                flatten
                vec)]
    (if (= grid res)
      false
      res)))

(def move-left (memoize unmemoized-move-left)) 

(defn move-down [grid]
  (-> grid
      rotate-grid
      move-left
      rotate-grid
      rotate-grid
      rotate-grid))

(defn move-right [grid]
  (-> grid
      rotate-grid
      rotate-grid
      move-left
      rotate-grid
      rotate-grid))

(defn move-up [grid]
  (-> grid
      rotate-grid
      rotate-grid
      rotate-grid
      move-left
      rotate-grid))

(defn show-game-over [app]
  (om/update! app [:game-over] "game-message game-over"))

(defn game-over? [grid]
  "return f on a full non-zero grid with no possible mergeable row/columns"
  ;; (let [col1 (utils/subset grid '(0 4 8 12))
  ;;       col2 (utils/subset grid '(1 5 9 13))
  ;;       col3 (utils/subset grid '(2 6 10 14))
  ;;       col4 (utils/subset grid '(3 7 11 15))
  ;;       row1 (subvec grid 0 4)
  ;;       row2 (subvec grid 4 8)
  ;;       row3 (subvec grid 8 12)
  ;;       row4 (subvec grid 12 16)]
  ;;   (every? (fn [lst]
  ;;             (loop [lst lst]
  ;;               (cond 
  ;;                (empty? lst) true
  ;;                (zero? (first lst)) false
  ;;                (= (first lst) (second lst)) false
  ;;                :else (recur (rest lst)))))
  ;;           (list col1 col2 col3 col4 row1 row2 row3 row4)))
  false)

(defn clear-grid [app]
  (om/update! app [:grid-values] [0 0 0 0
                                  0 0 0 0
                                  0 0 0 0
                                  0 0 0 0]))

(defn add-new-tile [app] 
  (let [grid (get @app :grid-values) 
        indexes (find-zero-indexes grid)
        chosen-idx (nth indexes (rand-int (count indexes)))
        new-grid (assoc grid chosen-idx (new-tile))]
    (om/update! app [:grid-values] new-grid)
    (if (game-over? new-grid)
      (show-game-over app))))

(defn restart-game [app]
  (om/update! app [:game-over] false)
  (clear-grid app) 
  (add-new-tile app)
  (add-new-tile app))

(defn move [app direction]
  (let [grid (get @app :grid-values)
        new-grid (cond (= direction :left) (move-left grid)
                       (= direction :right) (move-right grid)
                       (= direction :down) (move-down grid)
                       (= direction :up) (move-up grid))]
    (when new-grid
      (om/update! app [:grid-values] new-grid)
      (add-new-tile app))))
 
