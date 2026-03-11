(ns iris-layout.components.resizer
  (:require [reagent.core :as r]))

(defn resizer
  "Draggable split divider component"
  [direction split-id on-ratio-change]
  (let [dragging (r/atom false)]
    (fn [direction split-id on-ratio-change]
      [:div
       {:class (str "iris-resizer "
                    (name direction)
                    (when @dragging " dragging"))
        :on-mouse-down
        (fn [e]
          (.preventDefault e)
          (reset! dragging true)

          (let [target (.-currentTarget e)]
            (letfn [(handle-move [e]
                      (when @dragging
                        (let [parent (.-parentElement target)
                              rect (.getBoundingClientRect parent)
                              relative-pos (if (= direction :horizontal)
                                             (- (.-clientX e) (.-left rect))
                                             (- (.-clientY e) (.-top rect)))
                              size (if (= direction :horizontal)
                                     (.-width rect)
                                     (.-height rect))
                              new-ratio (/ relative-pos size)]
                          (on-ratio-change split-id new-ratio))))

                    (handle-up []
                      (reset! dragging false)
                      (.removeEventListener js/document "mousemove" handle-move)
                      (.removeEventListener js/document "mouseup" handle-up))]

              (.addEventListener js/document "mousemove" handle-move)
              (.addEventListener js/document "mouseup" handle-up))))}])))
