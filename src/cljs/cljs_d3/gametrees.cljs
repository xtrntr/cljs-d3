(ns cljs-d3.gametree
  (:require [cljs.core.async :refer [chan close! put! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljsjs.d3]
            [goog.string :as gstring]
            [goog.string.format])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]))

(defonce app-state (atom {:finished false}))

(defn timeout [ms] 
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn cell-color [val]
  (cond (= val 0) "rgba(250,248,239,1)"
        (= val 2) "#eee4da"
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

(defn text-color [val]
  (let [white "#f9f6f2"
        black "#776e65"]
    (if (> val 4) white black)))

(defn render-grid [g-element] )

(defn tree-viz [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:tree (clj->js {:parent "null"
                       :grid [0 0 0 8
                              2 0 0 16
                              2 0 0 32
                              0 0 0 64]
                       :children [{:grid [0 0 0 0
                                          0 0 0 0
                                          0 0 0 0
                                          4 0 0 0] 
                                   :parent "Top Level"}
                                  {:grid [4 0 0 0
                                          0 0 0 0
                                          0 0 0 0
                                          0 0 0 0] 
                                   :parent "Top Level"}
                                  {:grid [0 0 0 0
                                          0 0 0 2
                                          0 0 0 2
                                          0 0 0 0]
                                   :parent "Top Level"}]})
       :id 0})
    
    om/IDidMount
    (did-mount [_] 
      (let [margin (clj->js {:top 40
                             :right 120 
                             :bottom 20
                             :left 120})
            width (- 960 (.-right margin) (.-left margin))
            height (- 500 (.-top margin) (.-bottom margin))
            tree (.. js/d3.layout
                     tree
                     (size (clj->js [height width])))
            diagonal (.. js/d3.svg
                         diagonal
                         (projection (fn [d] (clj->js [(.-x d) (.-y d)]))))
            svg (.. js/d3
                    (select ".svg")
                    (append "svg")
                    (attr (clj->js {:width (+ width (.-right margin) (.-left margin))
                                    :height (+ height (.-top margin) (.-bottom margin))}))
                    (append "g")
                    (attr (clj->js {:transform (str "translate(" (.-left margin) "," (.-top margin) ")")})))
            root (om/get-state owner :tree)
            nodes (.. tree (nodes root))
            links (.. tree (links nodes))
            board-width 40
            board-height 40
            cell-width (/ board-width 4)
            cell-height (/ board-height 4)
            bg-color "rgb(255, 219, 122)"
            grid-color "#bbada0" ;"rgb(229, 197, 110)"
            ;; scale (.. js/d3.scale
            ;;           ordinal
            ;;           (domain (clj->js [1 2 3 4]))
            ;;           ;;# ordinal.rangeRoundBands(interval[, padding[, outerPadding]])
            ;;           (rangeBoundBands (clj->js [0 board-width]) 1 0.5))
            ]
        (.. nodes (forEach (fn [d] (set! (.-y d) (* 100 (.-depth d)))))) 
        (let [node (.. svg 
                       (selectAll "g.node")
                       (data nodes (fn [d]
                                     (let [curr-id (om/get-state owner :id)]
                                       (if (.-id d)
                                         (.-id d) 
                                         (do (set! (.-id d) curr-id)
                                             (om/set-state! owner :id (inc curr-id))
                                             (.-id d)))))))
              nodeEnter (.. node
                            enter
                            (append "g")
                            (attr (clj->js {:class "node"
                                            :transform (fn [d] 
                                                         (str "translate(" (.-x d) "," (.-y d) ")"))})))]
          ;; grid border
          (.. nodeEnter
              (append "rect")
              (attr (clj->js {:width board-width
                              :height board-height
                              :x 0 :y 0
                              :stroke grid-color
                              :fill "none"
                              :stroke-width 3})))
          (doseq [row (range 4)
                  col (range 4)]
            ;; cell
            (.. nodeEnter
                (append "svg:rect")
                (attr (clj->js {:width cell-width
                                :height cell-height 
                                :x (* row cell-width) :y (* col cell-height)
                                :stroke "none"
                                :fill (fn [d] (cell-color (aget (.-grid d) (+ row (* col 4)))))}))
                ;; cell border
                ;;(.. nodeEnter)
                (append "svg:rect")
                (attr (clj->js {:width cell-width
                                :height cell-height
                                :x (* row cell-width) :y (* col cell-height)
                                :stroke grid-color
                                :fill "none"
                                :stroke-width 1}))
                ;; cell text
                ;;(.. nodeEnter)
                (append "svg:text")
                (attr (clj->js {;:x (* (+ 0.2 row) cell-width) 
                                        ;:y (* (+ 0.6 col) cell-height)
                                :color (fn [d] (text-color (aget (.-grid d) (+ row (* col 4)))))
                                :font-family "Clear Sans, Helvetica Neue, Arial, sans-serif"
                                :font-size (/ cell-height 2)
                                :text-anchor "middle"}))  
                (text (fn [d] (aget (.-grid d) (+ row (* col 4)))))))
          ;; (.. nodeEnter
          ;;     (append "text")
          ;;     (attr (clj->js {:y (fn [d]
          ;;                          (if (or (.-children d) (.-_children d))
          ;;                            -18 18))
          ;;                     :dy ".35em"
          ;;                     :text-anchor "middle"}))
          ;;     (text (fn [d] (.-grid d)))
          ;;     (style "fill-opacity" 1))
          ;; (.. (.. svg
          ;;         (selectAll "path.link")
          ;;         (data links (fn [d] 
          ;;                       (str (.. d -source -id) "," (.. d -target -id)))))
          ;;     enter
          ;;     (insert "path" ".node-group")
          ;;     (attr (clj->js {:class "link"
          ;;                     :fill "none"
          ;;                     :stroke "#666666"
          ;;                     :stroke-width 2
          ;;                     :d (fn [d]
          ;;                          (let [o {:x (.. d -source -px)
          ;;                                   :y (.. d -source -py)}])
          ;;                          (diagonal {:source o
          ;;                                     :target o}))})))
          (.. (.. svg
                  (selectAll "path.link")
                  (data links (fn [d] 
                                (.. d -target -id))))
              enter
              (insert "path" "g")
              (attr (clj->js {:class "link"
                              :d diagonal})))
          )))

    om/IRender
    (render [this]
      (dom/div #js {:className "svg"}))

    ;; om/IDidUpdate
    ;; (did-update [_ _ _]
    ;;   (go (while (get @app :finished)
    ;;         (let []
    ;;           ;; refresh state
    ;;           (<! (timeout 5))
    ;;           ;; update state
    ;;           ))))
    ))
