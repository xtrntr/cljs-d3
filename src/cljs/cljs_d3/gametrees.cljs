(ns cljs-d3.gametree
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljsjs.d3]
            [goog.string :as gstring]
            [goog.string.format]
            [cljs-d3.gamelogic :as logic]
            [clojure.data :as data])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go go-loop]]))

(defn my-timeout [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(def row 0)
(def col 0)

(defn digits [n]
  (if (zero? n)
    '(0)
    (->> n
         (iterate #(quot % 10))
         (take-while pos?)
         (mapv #(mod % 10))
         count)))

(defn text-color [val]
  (let [white "#f9f6f2"
        black "#776e65"]
    (if (> val 4) white black)))

(def max-depth 4)
(def visited-nodes (clj->js []))
(def prev-node (clj->js {:parent "null"
                         :grid
                         [0 2 4 512
                          2 4 8 256
                          4 2 16 128
                          2 4 32 64]
                         }))
(def scores (clj->js []))
(def best-grid (clj->js {:score 0}))
(def node-counter 0)

;; push next-node into the array of all nodes.
(defn depth-first-search [owner]
  (let [curr-depth (om/get-state owner :depth)]
    (cond (zero? curr-depth) (do (.push visited-nodes prev-node)
                                 (om/set-state! owner :depth (inc curr-depth)))
          :else (let [visited-grids (js->clj (.map visited-nodes (fn [n] (.-grid n))))
                      possible-children (if (odd? curr-depth)
                                          (logic/generate-moves (js->clj (.-grid prev-node)))
                                          (logic/generate-spawns (js->clj (.-grid prev-node))))
                      valid-children (for [children possible-children
                                           :when (not (some #{children} visited-grids))]
                                       children)
                      has-children? (not (empty? valid-children))
                      going-down? (and has-children? (not (= curr-depth max-depth)))
                      probability (if (odd? curr-depth) 1
                                    (let [new-spawn (first (filter integer? (first (data/diff (first valid-children)
                                                                                              (js->clj (.-grid prev-node))))))]
                                      (if (= new-spawn 2) 0.9 0.1)))
                      next-node (if (even? curr-depth)
                                  (clj->js {:grid (first valid-children)
                                            :probability probability})
                                  (clj->js {:grid (first valid-children)}))
                      ;; we go up when there is no more children.
                      next-depth (if going-down?
                                   (inc curr-depth)
                                   (if (not has-children?)
                                     (dec curr-depth)
                                     curr-depth))]

                  (om/set-state! owner :depth next-depth)

                  (if going-down?
                    (do (when prev-node
                          (if (.. prev-node -children)
                            (.. prev-node -children (push next-node))
                            (set! (.-children prev-node) (clj->js [next-node]))))
                        (.push visited-nodes next-node)
                        (set! prev-node next-node))
                    (if has-children?
                      (do (when prev-node
                            (if (.. prev-node -children)
                              (.. prev-node -children (push next-node))
                              (set! (.-children prev-node) (clj->js [next-node]))))

                          (.push visited-nodes next-node))
                      (let [children-num (.. prev-node -children -length)
                            scores (for
                                     (let [obj (.pop visited-nodes)
                                           grid (.-grid obj)
                                           score (.-score obj)
                                           prob (.-probability obj)]
                                       (if (even? curr-depth)
                                         (do (when (> score (.-score best-grid))
                                               (set! best-grid obj))
                                             score)
                                         (* prob (logic/score-grid grid)))))
                            score (if (even? curr-depth)
                                     (/ (reduce + scores) children-num)
                                     (apply max scores))]
                        (set! (.. prev-node -score) score)
                        (set! (.. prev-node -children -length) 0)
                        (if (not (= (.-parent prev-node) "null"))
                          (set! prev-node (.-parent prev-node))
                          (do (set! (.-length visited-nodes) 0)
                              (set! prev-node (clj->js {:parent "null"
                                                        :grid (.-grid best-grid)}))
                              (.push visited-nodes prev-node)
                              (.log js/console "finished one step"))))))))))

(defn build-score-rect [node owner]
  (let [board-size (om/get-state owner :board-size)]
    (.. node
        (each (fn [d i]
                (this-as this
                  (when (and (not (= "null" (.-score d))) (.-score d))
                    (let [grid (.. js/d3
                                   (select this)
                                   (selectAll "g.grid")
                                   (data (clj->js [1])))]
                      (.. grid
                          enter
                          (append "svg:rect")
                          (attr (clj->js {:class "cover"
                                          :width board-size
                                          :height board-size
                                          ;; :fill-opacity "0.5"
                                          :x 0 :y 0})))
                      (.. grid
                          enter
                          (append "svg:text")
                          (attr (clj->js {:x (/ board-size 2)
                                          :y (/ board-size 2)
                                          :fill "#776e65"
                                          :font-family "Clear Sans, Helvetica Neue, Arial, sans-serif"
                                          :font-weight "Bold"
                                          :font-size 20
                                          :text-anchor "middle"}))
                          (text (gstring/format "%.3f" (.-score d))))))))))))

(defn build-2048-links [links owner]
  (.. links
      enter
      (insert "path" ".node-group")
      (attr (clj->js {:class "link"
                      :fill "none"
                      :stroke "#666666"
                      :stroke-width 5
                      :d (fn [d]
                           (let [o (clj->js {:x (.. d -source -px)
                                             :y (.. d -source -py)})]
                             ((om/get-state owner :diagonal) (clj->js {:source o
                                                                       :target o}))))}))))

(defn build-2048-rect [node-group owner]
  (defn cell-color [val]
    (cond (or (= val 0) (= val 2)) "#eee4da"
          (= val 4) "#ede0c8"
          (= val 8) "#f2b179"
          (= val 16) "#f59563"
          (= val 32) "#f67c5f"
          (= val 64) "#f65e3b"
          (= val 128) "#edcf72"
          (= val 256) "#edcc61"
          (= val 512) "#edc850"
          (= val 1024) "#edc53f"
          (= val 2048) "#edc22e"))
  (.. node-group
      (each (fn [d i]
              (this-as this
                (let [cell-size (om/get-state owner :cell-size)
                      width (om/get-state owner :width)
                      grid-color (om/get-state owner :grid-color)
                      cell (.. js/d3
                               (select this)
                               (selectAll "g.cell")
                               (data (.-grid d))
                               enter
                               (append "g")
                               (attr (clj->js {:class "node"
                                               :transform (fn [d]
                                                            (let [res (str "translate(" (* col cell-size) "," (* row cell-size) ")")]
                                                              (set! row (inc row))
                                                              (when (= row 4) (set! row 0) (set! col (inc col)))
                                                              (when (= col 4) (set! col 0))
                                                              res))})))]
                  (.. cell
                      (append "rect")
                      (attr (clj->js {:width cell-size
                                      :height cell-size
                                      :x 0 :y 0
                                      :stroke grid-color
                                      :stroke-width 1
                                      :fill-opacity (fn [d] (if (= d 0) 0.85 1))
                                      :fill (fn [d] (cell-color d))})))
                  (.. cell
                      (append "svg:text")
                      (attr (clj->js {:x (fn [d]
                                           (let [n (digits d)]
                                             (cond (= n 1) (* (+ 0.6 row) cell-size)
                                                   (= n 2) (* (+ 0.8 row) cell-size)
                                                   (= n 3) (* (+ 0.9 row) cell-size))))
                                      :y (* (+ 0.7 col) cell-size)
                                      :fill (fn [d] (text-color d))
                                      :font-family "Clear Sans, Helvetica Neue, Arial, sans-serif"
                                      :font-weight "Bold"
                                      :font-size (/ cell-size 2)
                                      :text-anchor "end"}))
                      (text (fn [d] (if (not (= 0 d)) d))))
                  cell))))))

(defn transition-and-exit [node links owner]
  (let [timeout (om/get-state owner :timeout)
        board-size (om/get-state owner :board-size)]
    (.. node
        transition
        (duration timeout)
        (attr (clj->js {:transform (fn [d]
                                     (set! (.-py d) (.-y d))
                                     (set! (.-px d) (.-x d))
                                     (str "translate(" (- (.. d -x) (/ board-size 2)) "," (- (.. d -y) (/ board-size 2)) ")"))})))
    (.. links
        transition
        (duration timeout)
        (attr (clj->js {:d (om/get-state owner :diagonal)})))
    (.. node
        exit
        transition
        (duration timeout)
        (attr (clj->js {:transform (fn [d]
                                     (str "translate(" (- (.. d -parent -px) (/ board-size 2)) "," (- (.. d -parent -py) (/ board-size 2)) ")")
                                     ;;(str "translate(" 0 "," 0 ")")
                                     )}))
        remove)
    (.. links
        exit
        transition
        (duration timeout)
        (attr (clj->js {:d (fn [d]
                             (let [o (clj->js {:x (.. d -source -x)
                                               :y (.. d -source -y)})]
                               ((om/get-state owner :diagonal) (clj->js {:source o
                                                                         :target o}))))}))
        remove)))

(defn tree-viz [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:margin (clj->js {:top 50
                         :right 50
                         :bottom 50
                         :left 50})
       :width 1020
       :height 820
       :board-size 100
       :cell-size 25
       :tree (.. js/d3.layout
                 tree
                 (size (clj->js [800 600])))
       :diagonal (.. js/d3.svg
                     diagonal
                     (projection (fn [d] (clj->js [(.-x d) (.-y d)]))))
       :bg-color "rgb(255, 219, 122)"
       :grid-color "#bbada0"
       :finished false
       :id 0
       :timer nil
       :timeout 250
       :depth 0
       :svg nil})

    om/IDidMount
    (did-mount [_]
      (let [timeout (om/get-state owner :timeout)
            tree (om/get-state owner :tree)]
        (.. tree (separation (fn [a b] 2)))
        (om/set-state! owner :svg (.. js/d3
                                      (select ".svg")
                                      (append "svg")
                                      (attr (clj->js {:width (om/get-state owner :width)
                                                      :height (om/get-state owner :height)}))
                                      (append "g")
                                      (attr (clj->js {:transform (str "translate(" 110 "," 110 ")")}))))
        (om/set-state! owner :timer
                       (js/setInterval (fn []
                                         (depth-first-search owner)
                                         (let [root (aget visited-nodes 0)
                                               svg (om/get-state owner :svg)
                                               board-size (om/get-state owner :board-size)
                                               cell-size (om/get-state owner :cell-size)
                                               width (om/get-state owner :width)
                                               grid-color (om/get-state owner :grid-color)
                                               root (aget visited-nodes 0)
                                               node (.. svg
                                                        (selectAll "g.node-group")
                                                        (data (.. tree (nodes root))
                                                              (fn [d]
                                                                (let [curr-id (om/get-state owner :id)]
                                                                  (if (.-id d)
                                                                    (.-id d)
                                                                    (do (set! (.-id d) curr-id)
                                                                        (om/set-state! owner :id (inc curr-id))
                                                                        (.-id d)))))))
                                               node-group (.. node
                                                              enter
                                                              (append "g")
                                                              (attr (clj->js {:class "node-group"
                                                                              :transform (fn [d]
                                                                                           (if (= "null" (.-parent d))
                                                                                             (str "translate(" (- (/ width 2) (/ board-size 2)) "," (- 0 (/ board-size 2)) ")")
                                                                                             (str "translate(" (- (.. d -parent -px) (/ board-size 2)) "," (- (.. d -parent -py) (/ board-size 2)) ")")))})))
                                               links (.. svg
                                                         (selectAll ".link")
                                                         (data (.. tree (links visited-nodes))
                                                               (fn [d] (str (.. d -source -id) "-" (.. d -target -id)))))]
                                           ;; these are the links between nodes
                                           (build-2048-links links owner)
                                           (build-2048-rect node-group owner)
                                           (build-score-rect node owner)

                                           (transition-and-exit node links owner)
                                           ;; (when (= "null" next-node)
                                           ;;   (js/clearInterval (om/get-state owner :timer))
                                           ;;   (om/update! owner :finished true))
                                           ))
                                       (+ 50 timeout)))))

    om/IRender
    (render [this]
      (dom/div #js {:className "svg"}))))
