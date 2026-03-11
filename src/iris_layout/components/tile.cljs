(ns iris-layout.components.tile
  (:require [reagent.core :as r]))

(defn calculate-half
  "Determine which half of the tile the cursor is closest to"
  [e tile-elem]
  (let [rect (.getBoundingClientRect tile-elem)
        cx (+ (.-left rect) (/ (.-width rect) 2))
        cy (+ (.-top rect) (/ (.-height rect) 2))
        dx (- (.-clientX e) cx)
        dy (- (.-clientY e) cy)]
    (if (> (js/Math.abs dx) (js/Math.abs dy))
      (if (neg? dx) :left :right)
      (if (neg? dy) :top :bottom))))

(defn half->direction [half]
  (if (or (= half :left) (= half :right))
    :horizontal
    :vertical))

(def direction-labels
  {:top "Split above"
   :bottom "Split below"
   :left "Split left"
   :right "Split right"})

(defn drop-indicator
  "Visual overlay showing where the drop will split"
  [half visible?]
  (when visible?
    [:div.iris-drop-indicator
     [:div {:class (str "iris-drop-highlight iris-drop-" (name half))}]
     [:div.iris-drop-label-container
      [:span.iris-drop-label (get direction-labels half "Split")]]]))

;; Global alt-key tracking (shared across all tiles)
(defonce alt-held (r/atom false))

(defonce _alt-listeners
  (do
    (.addEventListener js/document "keydown"
      (fn [e] (when (.-altKey e) (reset! alt-held true))))
    (.addEventListener js/document "keyup"
      (fn [e] (when (not (.-altKey e)) (reset! alt-held false))))
    (.addEventListener js/window "blur"
      (fn [_] (reset! alt-held false)))
    true))

(defn tile-component
  "Tile component with drag-drop and directional split overlay.
   on-split signature: (on-split target-tile-id entity-id direction source-type)
   source-type is :tile or :sidebar"
  [node on-split focused? entities render-entity]
  (let [drag-over (r/atom false)
        closest-edge (r/atom nil)
        dragging (r/atom false)
        split-ref (atom on-split)
        tile-ref (atom nil)]
    (fn [node on-split focused? entities render-entity]
      (reset! split-ref on-split)
      (let [entity (get entities (:entity-id node))]
        [:div
         {:ref #(reset! tile-ref %)
          :class (str "iris-tile"
                      (when focused? " iris-tile-focused")
                      (when (not focused?) " iris-tile-unfocused")
                      (when @drag-over " iris-drag-over")
                      (when @dragging " iris-dragging")
                      (when @alt-held " iris-tile-grabbable"))
          :style {:flex 1}
          :draggable true
          :on-drag-start
          (fn [e]
            (if (.-altKey e)
              (do
                (.setData (.-dataTransfer e) "text/plain"
                          (js/JSON.stringify #js {:tileId (:id node)
                                                  :entityId (:entity-id node)
                                                  :source "tile"}))
                (set! (.-effectAllowed (.-dataTransfer e)) "all")
                (reset! dragging true))
              (.preventDefault e)))
          :on-drag-end
          (fn [_e]
            (reset! dragging false))
          :on-drag-over
          (fn [e]
            (.preventDefault e)
            ;; Don't show overlay when dragging tile onto itself
            (if @dragging
              (do (reset! drag-over false)
                  (reset! closest-edge nil))
              (when-let [el @tile-ref]
                (let [half (calculate-half e el)]
                  (reset! closest-edge half)
                  (reset! drag-over true))))
)
          :on-drag-enter
          (fn [e]
            (.preventDefault e)
            (reset! drag-over true))
          :on-drag-leave
          (fn [e]
            (when (not (.contains (.-currentTarget e) (.-relatedTarget e)))
              (reset! drag-over false)
              (reset! closest-edge nil)))
          :on-drop
          (fn [e]
            (.preventDefault e)
            (.stopPropagation e)
            (when-let [el @tile-ref]
              (let [half (calculate-half e el)
                    direction (half->direction half)
                    raw (.getData (.-dataTransfer e) "text/plain")]
                (try
                  (let [data (js/JSON.parse raw)]
                    (cond
                      ;; Tile-to-tile rearrange
                      (and (= (.-source data) "tile") (.-entityId data))
                      (when (not= (.-tileId data) (:id node))
                        (@split-ref (:id node) (.-entityId data) direction :tile))

                      ;; Sidebar entity drag
                      (= (.-source data) "sidebar")
                      (when-let [eid (.-entityId data)]
                        (@split-ref (:id node) eid direction :sidebar))))
                  (catch :default _
                    (when (and raw (not= raw ""))
                      (@split-ref (:id node) raw direction :sidebar))))))
            (reset! drag-over false)
            (reset! closest-edge nil))}

         ;; Render entity
         (when (and entity render-entity)
           [:> render-entity entity])

         ;; Drop indicator overlay
         [drop-indicator @closest-edge @drag-over]]))))
