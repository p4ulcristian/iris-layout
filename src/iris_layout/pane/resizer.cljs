(ns iris-layout.pane.resizer
  (:require [reagent.core :as r]
            [reagent.hooks :as rh]))

(r/defc resizer
  "Draggable split divider component — supports mouse and touch."
  [direction split-id on-ratio-change]
  (r/with-let [callback-ref (atom on-ratio-change)]
    (reset! callback-ref on-ratio-change)
    (let [[dragging set-dragging] (rh/use-state false)
          start-resize
          (fn [target get-pos]
            (set-dragging true)
            (.add (.-classList js/document.body) "iris-resizing")
            (let [parent (.-parentElement target)]
              (.add (.-classList parent) "iris-dragging")
              (letfn [(calc-ratio [x y]
                        (let [rect (.getBoundingClientRect parent)
                              relative-pos (if (= direction "horizontal")
                                             (- x (.-left rect))
                                             (- y (.-top rect)))
                              size (if (= direction "horizontal")
                                     (.-width rect)
                                     (.-height rect))]
                          (/ relative-pos size)))

                      (handle-mouse-move [e]
                        (@callback-ref split-id (calc-ratio (.-clientX e) (.-clientY e))))

                      (handle-mouse-up [_e]
                        (set-dragging false)
                        (.remove (.-classList js/document.body) "iris-resizing")
                        (.remove (.-classList parent) "iris-dragging")
                        (.removeEventListener js/document "mousemove" handle-mouse-move)
                        (.removeEventListener js/document "mouseup" handle-mouse-up))

                      (handle-touch-move [e]
                        (.preventDefault e)
                        (let [touch (aget (.-changedTouches e) 0)]
                          (@callback-ref split-id (calc-ratio (.-clientX touch) (.-clientY touch)))))

                      (handle-touch-end [_e]
                        (set-dragging false)
                        (.remove (.-classList js/document.body) "iris-resizing")
                        (.remove (.-classList parent) "iris-dragging")
                        (.removeEventListener js/document "touchmove" handle-touch-move)
                        (.removeEventListener js/document "touchend" handle-touch-end))]

                (if get-pos
                  (do (.addEventListener js/document "touchmove" handle-touch-move #js {:passive false})
                      (.addEventListener js/document "touchend" handle-touch-end))
                  (do (.addEventListener js/document "mousemove" handle-mouse-move)
                      (.addEventListener js/document "mouseup" handle-mouse-up))))))]
      [:div
       {:class (str "iris-resizer "
                    direction
                    (when dragging " dragging"))
        :on-mouse-down
        (fn [e]
          (.preventDefault e)
          (.stopPropagation e)
          (start-resize (.-currentTarget e) nil))
        :on-touch-start
        (fn [e]
          (.preventDefault e)
          (.stopPropagation e)
          (start-resize (.-currentTarget e) :touch))}])))
