(ns cljs-d3.treemap
  (:require [cljs.core.async :refer [chan close! put! <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.d3]
            [cljsjs.papaparse])
  (:require-macros
   [cljs.core.async.macros :as m :refer [go]]))

(defn add1 [n] (inc n))
