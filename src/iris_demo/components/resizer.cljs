(ns iris-demo.components.resizer
  (:require [reagent.core :as r]))

(defn resizer
  "Draggable split divider component"
  [direction split-id on-ratio-change]
  (let [dragging (r/atom false)
        start-pos (r/atom 0)
        start-ratio (r/atom 0.5)]
    (fn [direction split-id on-ratio-change]
      [:div
       {:class (str "resizer "
                    (name direction)
                    (when @dragging " dragging"))
        :on-mouse-down
        (fn [e]
          (.preventDefault e)
          (reset! dragging true)
          (reset! start-pos (if (= direction :horizontal)
                              (.-clientX e)
                              (.-clientY e)))
          (reset! start-ratio 0.5)

          (letfn [(handle-move [e]
                    (when @dragging
                      (let [parent (.-parentElement (.-currentTarget e))
                            rect (.getBoundingClientRect parent)
                            current-pos (if (= direction :horizontal)
                                          (.-clientX e)
                                          (.-clientY e))
                            size (if (= direction :horizontal)
                                   (.-width rect)
                                   (.-height rect))
                            relative-pos (if (= direction :horizontal)
                                           (- (.-clientX e) (.-left rect))
                                           (- (.-clientY e) (.-top rect)))
                            new-ratio (/ relative-pos size)]
                        (on-ratio-change split-id new-ratio))))

                  (handle-up []
                    (reset! dragging false)
                    (.removeEventListener js/document "mousemove" handle-move)
                    (.removeEventListener js/document "mouseup" handle-up))]

            (.addEventListener js/document "mousemove" handle-move)
            (.addEventListener js/document "mouseup" handle-up)))}])))

(comment
  "Resizer allows dragging to adjust split ratios.
  - Horizontal splits have vertical resizer bars (col-resize)
  - Vertical splits have horizontal resizer bars (row-resize)
  - Ratio is clamped between 0.1 and 0.9 in layout.cljs")