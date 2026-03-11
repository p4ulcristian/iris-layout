(ns iris-layout.components.tile
  (:require [reagent.core :as r]))

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
  [node on-split focused? entities render-entity]
  (let [drag-over (r/atom false)]
    (fn [node on-split focused? entities render-entity]
      (let [entity (get entities (:entity-id node))]
        [:div
         {:class (str "iris-tile"
                      (when focused? " iris-tile-focused")
                      (when (not focused?) " iris-tile-unfocused")
                      (when @drag-over " iris-drag-over"))
          :style {:flex 1}
          :on-drag-over
          (fn [e]
            (.preventDefault e)
            (reset! drag-over true))
          :on-drag-leave
          (fn [_]
            (reset! drag-over false))
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
              (reset! drag-over false)))}

         ;; Render entity via user-provided function
         (when (and entity render-entity)
           [:> render-entity entity])]))))
