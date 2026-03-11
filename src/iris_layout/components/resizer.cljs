(ns iris-layout.components.resizer
  (:require [reagent.core :as r]))

(defn resizer
  "Draggable split divider component — supports mouse and touch."
  [direction split-id on-ratio-change]
  (let [dragging (r/atom false)
        callback-ref (atom on-ratio-change)]
    (fn [direction split-id on-ratio-change]
      (reset! callback-ref on-ratio-change)
      (let [start-resize
            (fn [target get-pos]
              (reset! dragging true)
              (.add (.-classList js/document.body) "iris-resizing")
              (let [parent (.-parentElement target)]
                ;; Add iris-dragging class to parent to disable flex transitions
                (.add (.-classList parent) "iris-dragging")
                (letfn [(calc-ratio [x y]
                          (let [rect (.getBoundingClientRect parent)
                                relative-pos (if (= direction :horizontal)
                                               (- x (.-left rect))
                                               (- y (.-top rect)))
                                size (if (= direction :horizontal)
                                       (.-width rect)
                                       (.-height rect))]
                            (/ relative-pos size)))

                        (handle-mouse-move [e]
                          (@callback-ref split-id (calc-ratio (.-clientX e) (.-clientY e))))

                        (handle-mouse-up [_e]
                          (reset! dragging false)
                          (.remove (.-classList js/document.body) "iris-resizing")
                          (.remove (.-classList parent) "iris-dragging")
                          (.removeEventListener js/document "mousemove" handle-mouse-move)
                          (.removeEventListener js/document "mouseup" handle-mouse-up))

                        (handle-touch-move [e]
                          (.preventDefault e)
                          (let [touch (aget (.-changedTouches e) 0)]
                            (@callback-ref split-id (calc-ratio (.-clientX touch) (.-clientY touch)))))

                        (handle-touch-end [_e]
                          (reset! dragging false)
                          (.remove (.-classList js/document.body) "iris-resizing")
                          (.remove (.-classList parent) "iris-dragging")
                          (.removeEventListener js/document "touchmove" handle-touch-move)
                          (.removeEventListener js/document "touchend" handle-touch-end))]

                  ;; Detect if touch or mouse based on caller
                  (if get-pos
                    (do (.addEventListener js/document "touchmove" handle-touch-move #js {:passive false})
                        (.addEventListener js/document "touchend" handle-touch-end))
                    (do (.addEventListener js/document "mousemove" handle-mouse-move)
                        (.addEventListener js/document "mouseup" handle-mouse-up))))))]

        [:div
         {:class (str "iris-resizer "
                      (name direction)
                      (when @dragging " dragging"))
          :on-mouse-down
          (fn [e]
            (.preventDefault e)
            (.stopPropagation e)
            (start-resize (.-currentTarget e) nil))
          :on-touch-start
          (fn [e]
            (.preventDefault e)
            (.stopPropagation e)
            (start-resize (.-currentTarget e) :touch))}]))))
