(ns cljs-d3.events
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as gevents]
            [cljs.core.async :refer [put! chan <! alts!]]
            ))

(def command-chan (chan))

(defn up []
  (put! command-chan :up))

(defn down []
  (put! command-chan :down))

(defn left []
  (put! command-chan :left))

(defn right []
  (put! command-chan :right))

(defn handle-transaction [tx-data root-cursor]
  (let [transaction-path (:path tx-data)] 
    (when (= (last transaction-path) :editing-frame)
      (put! command-chan :frame-switched))))

(def LEFT-KEY 37)
(def UP-KEY 38)
(def RIGHT-KEY 39)
(def DOWN-KEY 40)

(defn handle-key-event [app event] 
  (let [keyCode (.-keyCode event)
        handler (cond
                 ;; (= keycode LEFT-KEY) #(core/add-node app)
                 ;; (= keyCode LEFT-KEY)  #(logic/move app :left)
                 ;; (= keyCode UP-KEY)    #(logic/move app :up)
                 ;; (= keyCode RIGHT-KEY) #(logic/move app :right)
                 ;; (= keyCode DOWN-KEY)  #(logic/move app :down)
                 )]
    (when-not (= handler nil) (handler app))))

(defn handle-mouse-event [app owner event]
  (let [event-type (.-type event)
        mouse-down? (= event-type "mousedown")
        mouse-up? (= event-type "mouseup")
        svg (get @app :svg)]
    (when mouse-down?
      (this-as this
               (.. js/d3
                   (mouse svg))))))
      ;;  var point = d3.mouse(this))))

(defn key-listener [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:key-chan (chan)})

    om/IWillMount
    (will-mount [_]
      (let [key-chan (om/get-state owner :key-chan)] 
        (go
          (loop []
            (let [[v ch] (alts! [key-chan])]
              (when (= ch key-chan) (handle-key-event app v))
              (recur))))))

    om/IDidMount
    (did-mount [_]
      (let [key-chan (om/get-state owner :key-chan)]
        (gevents/listen js/document "keydown" #(put! key-chan %))))

    om/IRender
    (render [this]
      (dom/div nil ""))))

(defn mouse-listener
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:mouse-chan (chan)})
    
    ;go loop that handles event
    om/IWillMount
    (will-mount [_]
      (let [mouse-chan (om/get-state owner :mouse-chan)] 
        (go
          (loop []
            (let [[v ch] (alts! [mouse-chan])]
              (when (= ch mouse-chan) (handle-mouse-event app owner v))
              (recur))))))

    om/IDidMount
    (did-mount [_]
      (let [mouse-chan (om/get-state owner :mouse-chan)]
        (gevents/listen js/document "mousedown" #(put! mouse-chan %))
        (gevents/listen js/document "mouseup" #(put! mouse-chan %))
        (gevents/listen js/document "mousewheel" #(put! mouse-chan %))))
    
    om/IRender
    (render [this]
      (dom/div nil ""))))
