(ns iris-demo.components.tile
  (:require [reagent.core :as r]
            [iris-demo.components.entity :as entity]))

(defn get-drop-direction
  "Determine split direction based on mouse position in tile"
  [e tile-elem]
  (let [rect (.getBoundingClientRect tile-elem)
        x (- (.-clientX e) (.-left rect))
        y (- (.-clientY e) (.-top rect))
        width (.-width rect)
        height (.-height rect)
        x-ratio (/ x width)
        y-ratio (/ y height)]
    (cond
      (< y-ratio 0.5) :top
      (> y-ratio 0.5) :bottom
      (< x-ratio 0.5) :left
      :else :right)))

(defn tile-component
  "Tile component with drop zones for splitting"
  [node on-split focused? entities]
  (let [drag-over (r/atom false)
        drop-zone (r/atom nil)]
    (fn [node on-split focused? entities]
      (let [entity (get entities (:entity-id node))]
        [:div
         {:class (str "tile"
                      (when focused? " tile-focused")
                      (when (not focused?) " tile-unfocused")
                      (when @drag-over " drag-over"))
          :style {:flex 1}
          :on-drag-over
          (fn [e]
            (.preventDefault e)
            (reset! drag-over true)
            (let [direction (get-drop-direction e (.-currentTarget e))]
              (reset! drop-zone direction)))
          :on-drag-leave
          (fn [e]
            (reset! drag-over false)
            (reset! drop-zone nil))
          :on-drop
          (fn [e]
            (.preventDefault e)
            (let [entity-id (.getData (.-dataTransfer e) "text/plain")
                  direction (get-drop-direction e (.-currentTarget e))
                  split-dir (if (#{:top :bottom} direction)
                              :vertical
                              :horizontal)
                  before? (#{:top :left} direction)]
              (on-split (:id node) entity-id split-dir before?)
              (reset! drag-over false)
              (reset! drop-zone nil)))}

         ;; Entity content
         [entity/entity-renderer entity]]))))