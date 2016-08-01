(ns cljs-d3.chart
  (:require [cljs.core.async :refer [chan close! put! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.d3]
            [cljsjs.papaparse]
            [ajax.core :as ajax]
            [goog.string :as gstring])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]))

;; single line chart and getting data via ajax calls

(def data-url "https://data.gov.sg/api/action/datastore_search?resource_id=9326ca53-9153-4a9c-b93f-8ae032637b70")
(def once true)

(defn chart [app owner]
  (reify 
    om/IInitState
    (init-state [_]
      {:raw nil
       :fields []
       :records []})
    
    om/IDidMount
    (did-mount [_]
      (ajax/GET data-url 
                {:format :json
                 :response-format :json
                 :handler (fn [resp] (om/set-state! owner :raw resp))})
      (let [date-format (.. js/d3.time
                            (format "%d-%b-%y"))
            data (.. js/d3
                     (range 40)
                     (map #(if (mod % 5)
                             {:x (/ % 39)
                              :y (/ (+ 2 (.sin js/Math (/ % 3))) 4)}
                             nil)))
            margin (clj->js {:top 20 :right 20
                             :left 30 :bottom 50})
            width 890
            height 450
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
                     (x (fn [d] (x (.-date d))))
                     (y (fn [d] (y (.-close d)))))
            svg (.. js/d3
                    (select ".svg")
                    (append "svg")
                    (attr (clj->js {:width (+ width 50)
                                    :height (+ height 70)}))
                    (append "g") 
                    (attr (clj->js {:transform (str "translate(" 30 "," 20 ")")})))]
        (.. js/d3
            (tsv "data/data.tsv"  
                 (fn [d] 
                   (set! (.-date d) (.. date-format (parse (.-date d))))
                   (set! (.-close d) (gstring/toNumber (.-close d))) ;d.close = +d.close
                   d)
                 (fn [err data]
                   ;;(when err (throw (js/Error err)))
                   ;; x.domain(d3.extent(data, function(d) { return d.date; })); 
                   ;; y.domain(d3.extent(data, function(d) { return d.close; }));
                   (.. x (domain (.. js/d3 (extent data (fn [d] (.-date d))))))
                   (.. y (domain (.. js/d3 (extent data (fn [d] (.-close d))))))
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
                       (text "Price (%)"))
                   (.. svg
                       (append "path")
                       (datum data)
                       (attr (clj->js {:class "line"}))
                       (attr "d" line))
                   )))))
    
    om/IRender
    (render [_]
      (dom/div #js {:className "svg"}))

    om/IDidUpdate
    (did-update [_ _ _]
      (let [data (clj->js (om/get-state owner :raw))
            field-data-types (.-fields (.-result data))
            records (.-records (.-result data))
            next-record (.-next (.-_links (.-result data)))
            successful-query? (.-success data)
            id-type-hashmap (into {} (map (fn [field-pair] 
                                            {(keyword (.-id field-pair)) (.-type field-pair)})
                                          field-data-types))
            ids (keys id-type-hashmap)
            types (distinct (vals id-type-hashmap))
            records (get (last (last (om/get-state owner :raw))) "records")
            fields (get (last (last (om/get-state owner :raw))) "fields")]
        (when (and once data) 
          (set! once false)
          (ajax/GET (str "https://data.gov.sg" next-record) 
                    {:format :json
                     :response-format :json
                     :handler (fn [resp] 
                                (when (< 0 (count (get (last (last resp)) "records"))) 
                                  (set! once true))
                                (om/set-state! owner :raw resp))})
          (om/set-state! owner :fields (into (om/get-state owner :fields) fields))
          (om/set-state! owner :records (into (om/get-state owner :records) records))
          (println (map type
                        (list (om/get-state owner :raw)
                              (last (om/get-state owner :raw))
                              (last (last (om/get-state owner :raw)))
                              (get (last (last (om/get-state owner :raw))) "records"))))
          ))
      ;; (let [date-format (.. js/d3.time
      ;;                       (format "%d-%b-%y"))
      ;;       data (.. js/d3
      ;;                (range 40)
      ;;                (map #(if (mod % 5)
      ;;                        {:x (/ % 39)
      ;;                         :y (/ (+ 2 (.sin js/Math (/ % 3))) 4)}
      ;;                        nil)))
      ;;       margin (clj->js {:top 20 :right 20
      ;;                        :left 30 :bottom 50})
      ;;       width 890
      ;;       height 450
      ;;       x (.. js/d3.time
      ;;             scale
      ;;             (range (clj->js [0 width])))
      ;;       y (.. js/d3.scale
      ;;             linear
      ;;             (range (clj->js [height 0])))
      ;;       x-axis (.. js/d3.svg
      ;;                  axis
      ;;                  (scale x)
      ;;                  (orient "bottom"))
      ;;       y-axis (.. js/d3.svg
      ;;                  axis
      ;;                  (scale y)
      ;;                  (orient "left"))
      ;;       line (.. js/d3.svg
      ;;                line 
      ;;                (x (fn [d] (x (.-date d))))
      ;;                (y (fn [d] (y (.-close d)))))
      ;;       svg (.. js/d3
      ;;               (select ".svg")
      ;;               (append "svg")
      ;;               (attr (clj->js {:width (+ width 50)
      ;;                               :height (+ height 70)}))
      ;;               (append "g") 
      ;;               (attr (clj->js {:transform (str "translate(" 30 "," 20 ")")})))])
      )))
