(ns iris-layout.components.entity-tile
  "Entity tile component — renders a single entity inside the Body layout.

   Each tile is a leaf node in the layout tree. It:
   - Renders user content via the `render-entity-tile` callback
   - Supports Alt+drag to rearrange tiles within the layout
   - Shows directional drop overlay (split above/below/left/right)
   - Detects no-op rearrangements and hides the overlay

   Drag-drop protocol:
   - Tiles are draggable only while Alt is held (grab cursor appears)
   - On drag-start, serializes {:tileId :entityId :source \"tile\"} as JSON
   - On drag-over, calculates nearest edge and shows half-tile highlight
   - On drop, calls `on-split` with target tile-id, entity-id, direction, source-type"
  (:require [reagent.core :as r]))

(defn calculate-half
  "Determine which half of the tile the cursor is closest to.
   Returns :left, :right, :top, or :bottom."
  [e tile-elem]
  (let [rect (.getBoundingClientRect tile-elem)
        cx (+ (.-left rect) (/ (.-width rect) 2))
        cy (+ (.-top rect) (/ (.-height rect) 2))
        dx (- (.-clientX e) cx)
        dy (- (.-clientY e) cy)]
    (if (> (js/Math.abs dx) (js/Math.abs dy))
      (if (neg? dx) :left :right)
      (if (neg? dy) :top :bottom))))

(defn half->direction
  "Convert an edge half to a split direction.
   :left/:right → :horizontal, :top/:bottom → :vertical."
  [half]
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
   and the drop position would keep the same order.

   parent-ctx is {:direction :sibling-id :child-index} provided by the
   entity-tile-group (surface) component."
  [source-tile-id half parent-ctx]
  (when parent-ctx
    (let [{:keys [direction sibling-id child-index]} parent-ctx
          drop-dir (half->direction half)]
      (when (and (= source-tile-id sibling-id)
                 (= drop-dir direction))
        (or (and (= child-index 0) (or (= half :right) (= half :bottom)))
            (and (= child-index 1) (or (= half :left) (= half :top))))))))

(defn drop-indicator
  "Visual overlay showing where the drop will split.
   Renders a half-tile highlight with a label like 'Split above'."
  [half visible?]
  (when visible?
    [:div.iris-drop-indicator
     [:div {:class (str "iris-drop-highlight iris-drop-" (name half))}]
     [:div.iris-drop-label-container
      [:span.iris-drop-label (get direction-labels half "Split")]]]))

;; --- Global state (shared across all tiles) ---

(defonce alt-held
  (r/atom false))

(defonce _alt-listeners
  (do
    (.addEventListener js/document "keydown"
      (fn [e] (when (.-altKey e)
                (.preventDefault e)
                (reset! alt-held true))))
    (.addEventListener js/document "keyup"
      (fn [e] (when (not (.-altKey e)) (reset! alt-held false))))
    (.addEventListener js/window "blur"
      (fn [_] (reset! alt-held false)))
    true))

;; Track source tile ID globally so target tiles can detect no-ops
(defonce drag-source-tile (atom nil))

(defn entity-tile-component
  "Entity tile — a leaf node in the layout tree.

   Renders the user's content via `render-entity-tile` and handles all
   drag-drop interactions for tile rearrangement.

   Arguments:
   - node           : layout node {:type :tile :id str :entity-id str}
   - on-split       : fn(target-tile-id entity-id direction source-type)
   - focused?       : boolean, whether this tile has focus
   - entities       : map of entity-id → entity data
   - render-entity-tile : React component fn, receives entity data as props
   - parent-ctx     : {:direction :sibling-id :child-index} from parent split"
  [node on-split focused? entities render-entity-tile parent-ctx]
  (let [drag-over (r/atom false)
        closest-edge (r/atom nil)
        dragging (r/atom false)
        split-ref (atom on-split)
        tile-ref (atom nil)
        ctx-ref (atom parent-ctx)]
    (fn [node on-split focused? entities render-entity-tile parent-ctx]
      (reset! split-ref on-split)
      (reset! ctx-ref parent-ctx)
      (let [entity (get entities (:entity-id node))]
        [:div
         {:ref #(reset! tile-ref %)
          :class (str "iris-entity-tile"
                      (when focused? " iris-entity-tile-focused")
                      (when (not focused?) " iris-entity-tile-unfocused")
                      (when @drag-over " iris-drag-over")
                      (when @dragging " iris-dragging")
                      (when @alt-held " iris-entity-tile-grabbable"))
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
                          (@split-ref (:id node) (.-entityId data) direction :tile half)))

                      ;; Sidebar entity card drag
                      (= (.-source data) "sidebar")
                      (when-let [eid (.-entityId data)]
                        (@split-ref (:id node) eid direction :sidebar half))))
                  (catch :default _
                    (when (and raw (not= raw ""))
                      (@split-ref (:id node) raw direction :sidebar))))))
            (reset! drag-over false)
            (reset! closest-edge nil))}

         ;; Render entity content
         (when (and entity render-entity-tile)
           [:> render-entity-tile entity])

         ;; Drop indicator overlay
         [drop-indicator @closest-edge @drag-over]]))))
