(ns cljs-d3.gametree
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljsjs.d3]
            [goog.string :as gstring]
            [goog.string.format])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go go-loop]]))

(defn my-timeout [ms] 
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(def row 0)
(def col 0)

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

(def nodes (clj->js []))                    

(def next-nodes (clj->js [{:grid [0 2 4 512
                                  2 4 8 256
                                  4 2 16 128
                                  2 4 32 64]}
                          {:grid [2 2 4 512
                                  4 4 8 256
                                  2 2 16 128
                                  0 4 32 64]}
                          {:grid [2 4 512 0
                                  2 4 8 256
                                  4 2 16 128
                                  2 4 32 64]}
                          {:parent "null"
                           :grid [0 2 4 512
                                  2 4 8 256
                                  4 2 16 128
                                  2 4 32 64]}]))

(defn tree-viz [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:margin (clj->js {:top 50
                         :right 50 
                         :bottom 50
                         :left 50})
       :width 1020
       :height 720
       :board-size 100
       :cell-size 25
       :tree (.. js/d3.layout
                 tree
                 (size (clj->js [800 500])))
       :diagonal (.. js/d3.svg
                     diagonal
                     (projection (fn [d] (clj->js [(.-x d) (.-y d)]))))
       :bg-color "rgb(255, 219, 122)"
       :grid-color "#bbada0"
       :finished false
       :id 0
       :timer nil
       :timeout 750})
    
    om/IDidMount
    (did-mount [_]      
      (let [timeout (om/get-state owner :timeout)]
        (om/set-state! owner :svg (.. js/d3
                                      (select ".svg")
                                      (append "svg")
                                      (attr (clj->js {:width (om/get-state owner :width)
                                                      :height (om/get-state owner :height)}))
                                      (append "g")
                                      (attr (clj->js {:transform (str "translate(" 110 "," 110 ")")}))))
        (om/set-state! owner :timer 
                       (js/setInterval (fn [] 
                                         (let [tree (om/get-state owner :tree)
                                               next-node (.pop next-nodes)
                                               root (aget nodes 0)]
                                           (when root
                                             (if (.. root -children)  
                                               (.. root -children (push next-node))
                                               (set! (.-children root) (clj->js [next-node]))))
                                           (.push nodes next-node)
                                           (when (not next-node)
                                             (js/clearInterval (om/get-state owner :timer))
                                             (om/update! owner :finished true))
                                           (let [svg (om/get-state owner :svg)
                                                 board-size (om/get-state owner :board-size)
                                                 cell-size (om/get-state owner :cell-size)
                                                 width (om/get-state owner :width)
                                                 grid-color (om/get-state owner :grid-color)
                                                 root (aget nodes 0)
                                                 node (.. svg 
                                                          (selectAll "g.node-group")
                                                          (data (.. tree (nodes root)) (fn [d]
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
                                                                                :fill (str (om/get-state owner :id))
                                                                                :transform (fn [d] 
                                                                                             (if (= "null" (.-parent d))
                                                                                               (str "translate(" (- (/ width 2) (/ board-size 2)) "," (- 0 (/ board-size 2)) ")")
                                                                                               (str "translate(" (- (.. d -parent -px) (/ board-size 2)) "," (- (.. d -parent -py) (/ board-size 2)) ")")))})))
                                                 links (.. svg
                                                           (selectAll ".link") 
                                                           (data (.. tree (links nodes))  
                                                                 (fn [d] (str (.. d -source -id) "-" (.. d -target -id)))))]
                                             (.. links
                                                 enter
                                                 (insert "path" ".node-group")
                                                 (attr (clj->js {:class "link"
                                                                 :fill "none"
                                                                 :stroke "#666666"
                                                                 :stroke-width 2})))
                                             (.. node-group
                                                 (each (fn [segment i]
                                                         (this-as this
                                                                  (let [cell (.. js/d3
                                                                                 (select this)
                                                                                 (selectAll "g.cell")
                                                                                 (data (.-grid segment))
                                                                                 enter
                                                                                 (append "g")
                                                                                 (attr (clj->js {:class "node"
                                                                                                 :transform (fn [d] 
                                                                                                              (let [res (str "translate(" (* col cell-size) "," (* row cell-size) ")")]
                                                                                                                (set! row (inc row))
                                                                                                                (when (= row 4) (set! row 0) (set! col (inc col)))
                                                                                                                (when (= col 4) (set! col 0))
                                                                                                                res))})))]
                                                                    ;; cell border
                                                                    (.. cell
                                                                        (append "rect")
                                                                        (attr (clj->js {:width cell-size
                                                                                        :height cell-size
                                                                                        :x 0 :y 0
                                                                                        :stroke grid-color
                                                                                        :stroke-width 4
                                                                                        :fill-opacity (fn [d] (if (= d 0) 0.85 1))
                                                                                        :fill (fn [d] (cell-color d))
                                                                                        })))
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
                                                                    cell)))))
                                             (let [t (.. svg
                                                         transition
                                                         (duration timeout))]
                                               (.. t
                                                   (selectAll ".link")
                                                   (attr (clj->js {:d (om/get-state owner :diagonal)})))
                                               (.. t
                                                   (selectAll ".node-group")
                                                   (attr (clj->js {:transform (fn [d] 
                                                                                (set! (.-py d) (.-y d)) 
                                                                                (set! (.-px d) (.-x d))
                                                                                (str "translate(" (- (.. d -x) (/ board-size 2)) "," (- (.. d -y) (/ board-size 2)) ")"))})))))))
                                       timeout))))

    om/IRender
    (render [this]
      (dom/div #js {:className "svg"}))))
