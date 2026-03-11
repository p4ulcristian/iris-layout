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
        ;; Calculate distance from each edge as a fraction
        from-left (/ x width)
        from-top (/ y height)
        from-right (- 1 from-left)
        from-bottom (- 1 from-top)
        min-dist (min from-left from-top from-right from-bottom)]
    (cond
      (= min-dist from-top) :vertical
      (= min-dist from-bottom) :vertical
      (= min-dist from-left) :horizontal
      :else :horizontal)))

(defn tile-component
  "Tile component with drop zones for splitting"
  [node on-split focused? entities render-entity]
  (let [drag-over (r/atom false)
        split-ref (atom on-split)]
    (fn [node on-split focused? entities render-entity]
      (reset! split-ref on-split)
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
            (set! (.-dropEffect (.-dataTransfer e)) "copy")
            (reset! drag-over true))
          :on-drag-enter
          (fn [e]
            (.preventDefault e)
            (reset! drag-over true))
          :on-drag-leave
          (fn [e]
            ;; Only reset when leaving the tile itself, not its children
            (when (not (.contains (.-currentTarget e) (.-relatedTarget e)))
              (reset! drag-over false)))
          :on-drop
          (fn [e]
            (.preventDefault e)
            (.stopPropagation e)
            (let [entity-id (.getData (.-dataTransfer e) "text/plain")
                  direction (get-drop-direction e (.-currentTarget e))
                  before? false]
              (when (and entity-id (not= entity-id ""))
                (@split-ref (:id node) entity-id direction before?)))
            (reset! drag-over false))}

         ;; Render entity via user-provided function
         (when (and entity render-entity)
           [:> render-entity entity])]))))
