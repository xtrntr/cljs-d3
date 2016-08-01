(ns cljs-d3.multiline_chart
  (:require [cljs.core.async :refer [chan close! put! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.d3]
            [cljsjs.papaparse]
            [ajax.core :as ajax]
            [goog.string :as gstring])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]))

(defn multiline-chart [app owner]
  (reify
    
    om/IRender
    (render [_]
      (dom/div #js {:className "svg"}))
    
    om/IDidMount
    (did-mount [_]
      (let [margin (clj->js {:top 20 :right 80
                             :bottom 30 :left 50})
            width (- 960 (.-left margin) (.-right margin))
            height (- 500 (.-top margin) (.-bottom margin))
            ;; g (.. svg
            ;;       (append "g")
            ;;       (attr (clj->js {:transform (str "translate(" (.-left margin) "," (.-top margin) ")")})))
            date-format (.. js/d3.time
                            (format "%Y"))
            x (.. js/d3.time
                  scale
                  (range (clj->js [0 width])))
            y (.. js/d3.scale
                  linear
                  (range (clj->js [height 0])))
            x-axis (.. js/d3.svg
                       axis
                       (scale x)
                       (orient "bottom"))
            y-axis (.. js/d3.svg
                       axis
                       (scale y)
                       (orient "left"))
            line (.. js/d3.svg
                     line
                     (x (fn [d] (x (.-year d))))
                     (y (fn [d] (y (.-no_of_graduates d)))))
            svg (.. js/d3
                    (select ".svg")
                    (append "svg")
                    (attr (clj->js {:width (+ width 50)
                                    :height (+ height 70)}))
                    (append "g") 
                    (attr (clj->js {:transform (str "translate(" 30 "," 20 ")")})))]
        (.. js/d3
            (csv "data/graduates.csv"
                 (fn [d]
                   ;; check what are these arguments later
                   (set! (.-year d) (.. date-format (parse (.-year d))))
                   (set! (.-no_of_graduates d) (gstring/toNumber (.-no_of_graduates d)))
                   d)
                 (fn [err data]
                   (let [filtered-data (clj->js (filter (fn [entry] 
                                                          (println entry)
                                                          (when (and (not (some #{"Males" "na" "nan"} (vals entry)))
                                                                     (some #{"Engineering Sciences"} (vals entry))) entry)) 
                                                        (js->clj data)))]
                     (.. x (domain (.. js/d3 (extent filtered-data (fn [d] (.-year d))))))
                     (.. y (domain (.. js/d3 (extent filtered-data (fn [d] (.-no_of_graduates d))))))
                     (.. svg
                         (append "g")
                         (attr (clj->js {:class "x axis"
                                         :transform (str "translate(0," height ")")}))
                         (call x-axis))
                     (.. svg
                         (append "g")
                         (attr (clj->js {:class "y axis"}))
                         (call y-axis)
                         (append "text")
                         (attr (clj->js {:transform "rotate(-90)"
                                         :y 6
                                         :dy ".71em"}))
                         (style "text-anchor" "end")
                         (text "No. of graduates"))
                     (.. svg
                         (append "path")
                         (datum filtered-data)
                         (attr (clj->js {:class "line"}))
                         (attr "d" line))))))))
    
    ;; om/IDidUpdate
    ;; (did-update [_]
    ;;   )
))
