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

(defn- noop-rearrange?
  "Check if dropping source-tile-id at `half` of this tile would be a no-op.
   True when the source is the direct sibling in the same split direction
   and the drop position would keep the same order."
  [source-tile-id half parent-ctx]
  (when parent-ctx
    (let [{:keys [direction sibling-id child-index]} parent-ctx
          drop-dir (half->direction half)]
      (when (and (= source-tile-id sibling-id)
                 (= drop-dir direction))
        (or (and (= child-index 0) (or (= half :right) (= half :bottom)))
            (and (= child-index 1) (or (= half :left) (= half :top))))))))

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

;; Track source tile ID globally so target tiles can check no-op
(defonce drag-source-tile (atom nil))

(defn tile-component
  "Tile component with drag-drop and directional split overlay.
   on-split signature: (on-split target-tile-id entity-id direction source-type)
   source-type is :tile or :sidebar
   parent-ctx is {:direction :sibling-id :child-index} or nil"
  [node on-split focused? entities render-entity parent-ctx]
  (let [drag-over (r/atom false)
        closest-edge (r/atom nil)
        dragging (r/atom false)
        split-ref (atom on-split)
        tile-ref (atom nil)
        ctx-ref (atom parent-ctx)]
    (fn [node on-split focused? entities render-entity parent-ctx]
      (reset! split-ref on-split)
      (reset! ctx-ref parent-ctx)
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
                (reset! drag-source-tile (:id node))
                (reset! dragging true))
              (.preventDefault e)))
          :on-drag-end
          (fn [_e]
            (reset! dragging false)
            (reset! drag-source-tile nil))
          :on-drag-over
          (fn [e]
            (.preventDefault e)
            (if @dragging
              ;; Source tile — no overlay on self
              (do (reset! drag-over false)
                  (reset! closest-edge nil))
              (when-let [el @tile-ref]
                (let [half (calculate-half e el)
                      source-id @drag-source-tile
                      noop? (and source-id
                                 (noop-rearrange? source-id half @ctx-ref))]
                  (if noop?
                    (do (reset! drag-over false)
                        (reset! closest-edge nil))
                    (do (reset! closest-edge half)
                        (reset! drag-over true)))))))
          :on-drag-enter
          (fn [e]
            (.preventDefault e))
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
                      (let [source-id (.-tileId data)]
                        (when (and (not= source-id (:id node))
                                   (not (noop-rearrange? source-id half @ctx-ref)))
                          (@split-ref (:id node) (.-entityId data) direction :tile)))

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
