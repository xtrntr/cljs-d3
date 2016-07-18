(ns cljs-d3.core
  (:require [cljs.core.async :refer [chan close! put! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljsjs.d3]
            [cljs-d3.events :as events])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]))

(enable-console-print!)

(comment
  (def width  960)
  (def height 500)

;;;;;;;;;;;;;;;;;;;;;;
  ;; Three overlapping circles, based on example in the strokes repo:
  ;; https://github.com/dribnet/strokes/tree/master/examples/venn-simple
  ;; Passing an Object to d3's attr is allowed as a way of setting
  ;; multiple attributes in one call to attr.

  ;; Create an svg
  (def svg1 (.. js/d3
                (select "body")
                (append "svg")
                (attr (clj->js {:width width :height height}))))

  (.. svg1 
      (append "circle")
      (attr (clj->js {:cx 680 :cy 280 :r 150 :class "right"})))

  (.. svg1 
      (append "circle")
      (attr (clj->js {:cx 550 :cy 200 :r 175 :class "center"})))

  (.. svg1 
      (append "circle")
      (attr (clj->js {:cx 450 :cy 300 :r 200 :class "left"})))

;;;;;;;;;;;;;;;;;;
  ;; EXAMPLE 2
  ;; Translation with minor modifications of Bostock's general update pattern I 
  ;; example: http://bl.ocks.org/mbostock/3808218

  (def alphabet (map char (range 97 (+ 97 26)))) ; lowercase letters a through z
  ;; Create a g attached to a new svg; we'll add the letters to the g.
  (def svg2g (.. js/d3 
                 (select "body")
                 (append "svg")
                 (attr (clj->js {:width width :height height}))
                 (append "g") ; so we can move the letters to a nice place in svg
                 (attr "transform" (str "translate(32," (/ height 4) ")"))))

  (defn d3update 
  "Given data in a Clojurescript sequence, converts it to a Javascript array
  and uses it to update the previous d3 display--either by creating something
  where there was nothing, by adding to it, by shortening it, etc."
  [clj-data]

  ;; Join new data with old elements, if any.
  (let [data (clj->js clj-data) ; Important: convert Clojure collection to Javascript array.
        text (.. svg2g 
                 (selectAll "text") 
                 (data data))]
    ;; Update old elements as needed.
    (.attr text "class" "update") ; i.e. since they're old, we reclassify them
    ;; Create new elements as needed.
    (.. text 
        (enter)
        (append "text")
        (attr (clj->js {:class "enter"     ; new elts have a different class than old
                        :x     #(* %2 32)  ; move each element 32px right of preceding
                        :dy    ".35em"}))) ; see note above about clj->js and maps
    ;; Appending to the enter selection expands the update selection to include
    ;; entering elements; so, operations on the update selection after appending to
    ;; the enter selection will apply to both entering and updating nodes.
    (.text text identity) ; display letter that's the data elt
    ;; Remove old elements as needed.
    (.. text (exit) (remove)))) ; strip out the ones that didn't match

  (d3update alphabet)

  ;; Function that will be called repeatedly after a delay to change the display.
  (defn intervalfn
  "Selects a random subset of letters from alphabet, alphabetizes the result,
  and passes it to update."
  []
  (-> alphabet
      shuffle
      (subvec (rand-int 26)) ; since we shuffled the seq, we can just take the rest starting from a rand int
      sort       ; put 'em back in alphabetical order
      d3update)) ; will convert it to a Javascript array before doing anything

  ;; Call the intervalfn function after some number of milliseconds.
  ;; Note the window parameter wasn't needed in the original Javascript, but
  ;; we need it here.  Not sure why there's a difference.

  (.setInterval js/window intervalfn 1500))

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
 
;; force-layout.nodes(nodes).links(links).start()
(defn restart-force-layout [force nodes links]
  (.. force
      (nodes nodes)
      (links links)
      start))

(def selected_link nil)
(def selected_node)

;; svg.selectAll("link").data
(defn build-links [path links]
  (.. path
      (data links)
      enter
      (append "svg:path") 
      (attr (clj->js {:class "link" :stroke "black"
                      :selected (fn [d] (= d selected_link))}))
      (style "marker-start" (fn [d] (if (.-left d) "url(#start-arrow)" "")))
      (style "marker-end" (fn [d] (if (.-right d) "url(#end-arrow)" "")))))

(defn build-nodes [circle nodes force-layout]
  (.. circle
      (data nodes)
      enter
      (append "svg:g")
      (append "svg:circle")
      (attr (clj->js {:class "node" :fill "blue" 
                      :stroke "black" :r 15}))
      (call (.-drag force-layout))))

(defn on-tick [links nodes]
  (fn []
    (.. links
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
                      (str "M " sourceX "," sourceY " L " targetX "," targetY)))))
    (.. nodes
        (attr "transform" #(str "translate(" (.. % -x) "," (.. % -y) ")")))))

(defn svg [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:path nil
       :circle nil
       :force nil})
    om/IDidMount
    (did-mount [_]
      (let [svg (build-svg 960 600)
            path (.. svg (append "svg:g") (selectAll ".path")) 
            circle (.. svg (append "svg:g") (selectAll ".g"))
            force (build-force-layout 960 600)
            nodes (get @app :nodes)
            links (get @app :links)]
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
                            :orient "auto"}))
            (append "svg:path")
            (attr (clj->js {:d "M10,-5 L0,0 L10,5" 
                            :fill "#000"})))
        (.. svg
            (on "mousedown" 
                (fn [d] (this-as this 
                                 (let [point (.mouse js/d3 this)
                                       x (aget point 0)
                                       y (aget point 1)]
                                   (om/update! app [:nodes] 
                                               (clj->js (conj (js->clj (get @app :nodes)) {:id 123
                                                                                           :x x :y y}))) 
                                   )))))
        (.on force "tick"
             (on-tick (build-links path links)
                      (build-nodes circle nodes force)))
        (restart-force-layout force nodes links)

        (om/update! app [:svg] svg)
        (om/update-state! owner #(assoc % :path path))
        (om/update-state! owner #(assoc % :circle circle))))

    om/IRender
    (render [this]
      (dom/div #js {:className "svg"}))

    om/IDidUpdate
    (did-update [_ _ _]
      (let [nodes (get @app :nodes)
            links (get @app :links)
            svg (get @app :svg)
            force (build-force-layout 960 600)]
        (println (get @app :nodes))
        (restart-force-layout force nodes links)
        ))))

(defn screen [app owner]
  (om/component
   (dom/div nil
            (om/build events/key-listener app)
            (om/build svg app))))

(def app-state (atom {:nodes (clj->js [{:id 0 :reflexive true}
                                       {:id 1 :reflexive false}
                                       {:id 2 :reflexive true}])
                      :links (clj->js [{:source 0 :target 1 :left false :right true}
                                       {:source 1 :target 2 :left false :right true}])})) 

(om/root 
 screen
 app-state 
 {:target (js/document.getElementById "app")})
