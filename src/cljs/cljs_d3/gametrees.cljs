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

(def row 0)
(def col 0)

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
                       :grid [0 2 4 512
                              2 4 8 256
                              4 2 16 128
                              2 4 32 64]
                       :children [{:grid  [2 2 4 512
                                           4 4 8 256
                                           2 2 16 128
                                           0 4 32 64]
                                   :parent "Top Level"}
                                  {:grid [2 4 512 0
                                          2 4 8 256
                                          4 2 16 128
                                          2 4 32 64]
                                   :parent "Top Level"}]})
       :id 0})
    
    om/IDidMount
    (did-mount [_] 
      (let [margin (clj->js {:top 50
                             :right 50 
                             :bottom 50
                             :left 50})
            width (- 960 (.-right margin) (.-left margin))
            height (- 500 (.-top margin) (.-bottom margin))
            board-size 40
            cell-size (/ board-size 4)
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
                    (attr (clj->js {:transform (str "translate(" (.-left margin) "," (.-top margin) ")")}))
                    )
            root (om/get-state owner :tree)
            nodes (.. tree (nodes root))
            links (.. tree (links nodes))
            bg-color "rgb(255, 219, 122)"
            grid-color "#bbada0"]
        (set! (.-py root) (.-y root))
        (set! (.-px root) (.-x root))
        (.. nodes (forEach (fn [d] (set! (.-y d) (* 100 (.-depth d)))))) 
        (let [node (.. svg 
                       (selectAll "g.node-group")
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
                            (attr (clj->js {:class "node-group"
                                            :transform (fn [d] 
                                                         (str "translate(" (.-x d) "," (.-y d) ")")
                                                         ;; (if (= "null" (.-parent d))
                                                         ;;   (do (.log js/console (- (/ width 2) (/ board-size 2))) 
                                                         ;;       (str "translate(" (- (/ width 2) (/ board-size 2)) "," (- 0 (/ board-size 2)) ")"))
                                                         ;;   (do (.log js/console (- (.. d -parent -y) (/ board-size 2))) 
                                                         ;;       (str "translate(" (- (.. d -parent -x) (/ board-size 2)) "," (- (.. d -parent -y) (/ board-size 2)) ")")))
                                                         )})))]
          (.. nodeEnter
              (each (fn [segment i]
                      (this-as this
                               (let [cell (.. js/d3
                                              (select this)
                                              (selectAll "g.cell")
                                              (data (.-grid segment))
                                              enter
                                              (append "g")
                                              (attr (clj->js {:class "node"
                                                              :row 0
                                                              :transform (fn [d] 
                                                                           (let [res (str "translate(" (* col 10) "," (* row 10) ")")]
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
                                                     :fill (fn [d] (cell-color d))
                                                     })))
                                 (.. cell
                                     (append "svg:text")
                                     (attr (clj->js {:x (* (+ 0.8 row) cell-size) 
                                                     :y (* (+ 0.6 col) cell-size)
                                                     :color (fn [d] (text-color d))
                                                     :font-family "Clear Sans, Helvetica Neue, Arial, sans-serif"
                                                     :font-size (/ cell-size 2)
                                                     :text-anchor "end"}))
                                     (text (fn [d] d)))
                                 cell)))))
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

    om/IDidUpdate
    (did-update [_ _ _]
      (go (while (get @app :finished)
            (let []
              ;; refresh state
              (<! (timeout 5000))
              ;; update state
              ))))
    ))
