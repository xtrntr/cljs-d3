(ns cljs-d3.core
  (:require [cljs.core.async :refer [chan close! put! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljsjs.d3]
            [goog.string :as gstring]
            [goog.string.format]
            [cljs-d3.chart :as chart]
            [cljs-d3.force :as force]
            [cljs-d3.gametree :as gametree]
            [cljs-d3.multiline_chart :as multiline])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]))

(defn screen [app owner]
  (om/component
   (dom/div nil
            ;(om/build multiline/multiline-chart app)
            ;(om/build chart/chart app)
            ;(om/build force/force-chart app)
            (om/build gametree/tree-viz app)
            )))

(def app-state (atom {:svg nil
                      :path nil
                      :circle nil
                      :force-layout nil}))

(om/root 
 screen
 app-state 
 {:target (js/document.getElementById "app")})
 
