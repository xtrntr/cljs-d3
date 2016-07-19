(ns cljs-d3.core
  (:require [cljs.core.async :refer [chan close! put! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljsjs.d3]
            [cljs-d3.events :as events]
            [goog.string :as gstring]
            [goog.string.format])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]))

(enable-console-print!)

;; d3.layout.force.charge(-140).linkdistance(40).size([width height])
(defn build-force-layout [width height]
  (.. js/d3.layout
      force
      (charge -500)
      (linkDistance 150)
      (size (array width height))))

(defn build-svg [width height]
  (.. js/d3
      (select ".svg")
      (append "svg")
      (attr (clj->js {:width width :height height
                      :oncontextmenu "return false;"}))))

(def lastnodeID 2)

(defn get-node [owner idx]
  ;(.log js/console "i am being called")
  (aget (:nodes (om/get-state owner)) idx))

(defn svg-component [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:nodes (clj->js [{:id 0 :reflexive true}
                        {:id 1 :reflexive false}
                        {:id 2 :reflexive true}])
       :links nil})
    
    ;;Called once when the component has been mounted into the DOM. 
    ;;The DOM node associated with this component can be retrieved by using (om.core/get-node owner).
    om/IDidMount
    (did-mount [_]
      (om/set-state! owner :links (clj->js [{:source (get-node owner 0) :target (get-node owner 1) :left false :right true}
                                            {:source (get-node owner 1) :target (get-node owner 2) :left false :right true}]))
      (let [nodes (om/get-state owner :nodes)
            links (om/get-state owner :links)
            svg (build-svg 960 600)
            path (.. svg (append "svg:g") (selectAll "path"))
            circle (.. svg (append "svg:g") (selectAll "g"))
            force-layout (build-force-layout 960 600)]
        ;; define arrow markers for graph links
        (.. svg 
            (append "svg:defs")
            (append "svg:marker")
            (attr (clj->js {:id "end-arrow"
                            :viewBox "0 -5 10 10"
                            :refX 6 
                            :markerWidth 3
                            :markerHeight 3
                            :orient "auto"}))
            (append "svg:path") 
            (attr (clj->js {:d "M0,-5 L10,0 L0,5"
                            :fill "#000"})))
        (.. svg 
            (append "svg:defs")
            (append "svg:marker")
            (attr (clj->js {:id "start-arrow"
                            :viewBox "0 -5 10 10"
                            :refX 4
                            :markerWidth 3
                            :markerHeight 3
                            :orient "auto" ;look up later
                            }))
            (append "svg:path")
            (attr (clj->js {:d "M10,-5 L0,0 L10,5" 
                            :fill "#000"})))
        (.. svg
            ;; add new nodes
            (on "mousedown" 
                (fn [d] 
                  (this-as this 
                           (let [point (.mouse js/d3 this)
                                 x (aget point 0)
                                 y (aget point 1)]
                             (set! lastnodeID (inc lastnodeID))
                             (om/set-state! owner :nodes 
                                            (clj->js (conj (js->clj (om/get-state owner :nodes)) {:id lastnodeID
                                                                                                  :x x :y y}))))))))
        (.on force-layout "tick" (fn []
                                   (.. (get @app :circle)
                                       (attr "transform" #(str "translate(" (.. % -x) "," (.. % -y) ")")))
                                   (.. (.. (get @app :path) (data (om/get-state owner :links)))
                                       (attr "d" (fn [d] 
                                                   (let [deltaX (- (.. d -target -x)
                                                                   (.. d -source -x))
                                                         deltaY (- (.. d -target -y)
                                                                   (.. d -source -y))
                                                         dist (.sqrt js/Math (+ (* deltaX deltaX) (* deltaY deltaY)))
                                                         normX (/ deltaX dist)
                                                         normY (/ deltaY dist)
                                                         sourcePadding (if (.-left d) 17 12)
                                                         targetPadding (if (.-right d) 17 12)
                                                         sourceX (+ (.. d -source -x) (* sourcePadding normX))
                                                         sourceY (+ (.. d -source -y) (* sourcePadding normY))
                                                         targetX (- (.. d -target -x) (* targetPadding normX))
                                                         targetY (- (.. d -target -y) (* targetPadding normY))]
                                                     (.log js/console (str "d: " (.. d -source)
                                                                           "\nnodes: " (aget (aget (om/get-state owner :nodes) 0) "x")
                                                                           "\nlinks: " (aget (aget (aget (om/get-state owner :links) 0) "source") "x")))
                                                     ;; (println (str  "bad: " 
                                                     ;;                (gstring/format "%.2f" (.. d -source -x)) 
                                                     ;;                " test: " 
                                                     ;;                (gstring/format "%.2f" (aget (aget (om/get-state owner :nodes) 0) "x"))))
                                        ;(println (.. d -source))
                                                     (str "M " sourceX "," sourceY " L " targetX "," targetY)))))))
        (om/update! app [:path] path)
        (om/update! app [:circle] circle)
        (om/update! app [:svg] svg) 
        (om/update! app [:force-layout] force-layout)))

    ;;div element
    om/IRender
    (render [this]
      (dom/div #js {:className "svg"}))

    ;;
    om/IDidUpdate
    (did-update [_ _ _]
      (let [nodes (om/get-state owner :nodes)
            links (om/get-state owner :links)
            svg (get @app :svg)
            path (.. (get @app :path) (data links))
            circle (.. (get @app :circle) (data nodes (fn [d] (.-id d))))
            force-layout (get @app :force-layout)]
                                        ;(.. svg (selectAll ".node") remove)
                                        ;(.. svg (selectAll ".path") remove)
                                        ;(println (str "wrongy " (js->clj (.. (get @app :path) (data links)))))
                                        ;(println (str "righty " (js->clj links)))
                                        ;(println links)
        (om/update! app [:path] path)
        (om/update! app [:circle] circle)
        ;; rebuild links
        (om/set-state! owner :links (clj->js [{:source (get-node owner 0) :target (get-node owner 1) :left false :right true}
                                              {:source (get-node owner 1) :target (get-node owner 2) :left false :right true}]))
        
        (.. path
            (style "marker-start" (fn [d] (if (.-left d) "url(#start-arrow)" "")))
            (style "marker-end" (fn [d] (if (.-right d) "url(#end-arrow)" ""))))
        (.. path
            enter
            (append "svg:path")
            (attr (clj->js {:class "link" :stroke "black"}))
            (style "marker-start" (fn [d] (if (.-left d) "url(#start-arrow)" "")))
            (style "marker-end" (fn [d] (if (.-right d) "url(#end-arrow)" ""))))
        (.. path exit remove)

        (.. circle (selectAll "circle"))
        (let [g (.. circle enter (append "svg:g"))]
          (.. g
              (append "svg:circle")
              (attr (clj->js {:class "node" :fill "blue" 
                              :stroke "black" :r 15})))
          (.. g
              (append "svg:text")
              (attr (clj->js {:x 0 :y 4
                              :class "id"})) 
              (text (fn [d] (.-id d)))))
                                        ;(println (str "true: " (aget (aget nodes 0) "x")))
        ;(.. circle exit remove)
        (.. force-layout
            (nodes nodes)
            (links links)
            start)))))

(defn screen [app owner]
  (om/component
   (dom/div nil
            (om/build events/key-listener app)
            (om/build svg-component app))))

(def app-state (atom {:svg nil
                      :path nil
                      :circle nil
                      :force-layout nil}))

(om/root 
 screen
 app-state 
 {:target (js/document.getElementById "app")})
