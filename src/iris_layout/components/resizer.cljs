(ns iris-layout.components.resizer
  (:require [reagent.core :as r]))

(defn resizer
  "Draggable split divider component"
  [direction split-id on-ratio-change]
  (let [dragging (r/atom false)
        ;; Keep a stable ref to latest callback
        callback-ref (atom on-ratio-change)]
    (fn [direction split-id on-ratio-change]
      (reset! callback-ref on-ratio-change)
      [:div
       {:class (str "iris-resizer "
                    (name direction)
                    (when @dragging " dragging"))
        :on-mouse-down
        (fn [e]
          (.preventDefault e)
          (.stopPropagation e)
          (reset! dragging true)

          (let [target (.-currentTarget e)
                parent (.-parentElement target)]
            (letfn [(handle-move [e]
                      (let [rect (.getBoundingClientRect parent)
                            relative-pos (if (= direction :horizontal)
                                           (- (.-clientX e) (.-left rect))
                                           (- (.-clientY e) (.-top rect)))
                            size (if (= direction :horizontal)
                                   (.-width rect)
                                   (.-height rect))
                            new-ratio (/ relative-pos size)]
                        (@callback-ref split-id new-ratio)))

                    (handle-up [e]
                      (reset! dragging false)
                      (.removeEventListener js/document "mousemove" handle-move)
                      (.removeEventListener js/document "mouseup" handle-up))]

              (.addEventListener js/document "mousemove" handle-move)
              (.addEventListener js/document "mouseup" handle-up))))}])))
