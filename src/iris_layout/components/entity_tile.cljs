(ns iris-layout.components.entity-tile
  "Entity tile component — renders a single entity inside the Body layout.

   Each tile is a leaf node in the layout tree. It:
   - Renders a header bar with entity name, drag handle, and close button
   - Renders user content via the `render-entity-tile` callback
   - Desktop: header is the drag handle (HTML5 Drag and Drop)
   - Mobile: long-press anywhere on tile to start touch drag
   - Shows directional drop overlay (split above/below/left/right)
   - Detects no-op rearrangements and hides the overlay"
  (:require [reagent.core :as r]
            [iris-layout.components.touch-drag :as touch-drag]))

(defn calculate-half
  "Determine which half of the tile the cursor is closest to."
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
  "Convert an edge half to a split direction."
  [half]
  (if (or (= half :left) (= half :right))
    :horizontal
    :vertical))

(def direction-labels
  {:top "Split above" :bottom "Split below"
   :left "Split left" :right "Split right"})

(defn- noop-rearrange?
  [source-tile-id half parent-ctx]
  (when parent-ctx
    (let [{:keys [direction sibling-id child-index]} parent-ctx
          drop-dir (half->direction half)]
      (when (and (= source-tile-id sibling-id)
                 (= drop-dir direction))
        (or (and (= child-index 0) (or (= half :right) (= half :bottom)))
            (and (= child-index 1) (or (= half :left) (= half :top))))))))

(defn drop-indicator [half visible?]
  (when visible?
    [:div.iris-drop-indicator
     [:div {:class (str "iris-drop-highlight iris-drop-" (name half))}]
     [:div.iris-drop-label-container
      [:span.iris-drop-label (get direction-labels half "Split")]]]))

;; Track source tile ID globally so target tiles can detect no-ops
(defonce drag-source-tile (atom nil))

;; Fullscreen tile — only one tile can be fullscreen at a time
(defonce fullscreen-tile (r/atom nil))

(defn entity-tile-component
  [node on-split on-close focused? entities render-entity-tile parent-ctx]
  (let [drag-over (r/atom false)
        closest-edge (r/atom nil)
        dragging (r/atom false)
        split-ref (atom on-split)
        close-ref (atom on-close)
        tile-ref (atom nil)
        ctx-ref (atom parent-ctx)
        ;; Watch touch hover for this tile
        touch-watch-key (str "tile-" (:id node))
        _ (add-watch touch-drag/hover-target touch-watch-key
            (fn [_ _ _ new-target]
              (if (and new-target (= (:tile-id new-target) (:id node)))
                (let [half (:half new-target)
                      source-id (:tile-id @touch-drag/touch-state)
                      noop? (and source-id
                                 (noop-rearrange? source-id half @ctx-ref))]
                  (if (or noop? (= source-id (:id node)))
                    (do (reset! drag-over false)
                        (reset! closest-edge nil))
                    (do (reset! closest-edge half)
                        (reset! drag-over true))))
                (when @drag-over
                  (reset! drag-over false)
                  (reset! closest-edge nil)))))]
    (r/create-class
      {:component-will-unmount
       (fn [_]
         (remove-watch touch-drag/hover-target touch-watch-key))

       :reagent-render
       (fn [node on-split on-close focused? entities render-entity-tile parent-ctx]
         (reset! split-ref on-split)
         (reset! close-ref on-close)
         (reset! ctx-ref parent-ctx)
         (let [entity (get entities (:entity-id node))
               entity-name (or (:name entity) (:entity-id node))]
           [:div
            {:ref #(reset! tile-ref %)
             :data-tile-id (:id node)
             :class (str "iris-entity-tile"
                         (when focused? " iris-entity-tile-focused")
                         (when (not focused?) " iris-entity-tile-unfocused")
                         (when @drag-over " iris-drag-over")
                         (when @dragging " iris-dragging")
                         (when (= @fullscreen-tile (:id node)) " iris-entity-tile-fullscreen"))
             :style {:flex 1}

             ;; Mouse drag-over/drop handlers
             :on-drag-over
             (fn [e]
               (.preventDefault e)
               (if @dragging
                 (do (reset! drag-over false)
                     (reset! closest-edge nil))
                 (when-let [el @tile-ref]
                   (let [half (calculate-half e el)
                         source-id @drag-source-tile
                         noop? (and source-id (noop-rearrange? source-id half @ctx-ref))]
                     (if noop?
                       (do (reset! drag-over false) (reset! closest-edge nil))
                       (do (reset! closest-edge half) (reset! drag-over true)))))))
             :on-drag-enter (fn [e] (.preventDefault e))
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
                         (and (= (.-source data) "tile") (.-entityId data))
                         (let [source-id (.-tileId data)]
                           (when (and (not= source-id (:id node))
                                      (not (noop-rearrange? source-id half @ctx-ref)))
                             (@split-ref (:id node) (.-entityId data) direction :tile half)))
                         (= (.-source data) "sidebar")
                         (when-let [eid (.-entityId data)]
                           (@split-ref (:id node) eid direction :sidebar half))))
                     (catch :default _
                       (when (and raw (not= raw ""))
                         (@split-ref (:id node) raw direction :sidebar))))))
               (reset! drag-over false)
               (reset! closest-edge nil))

             ;; Touch: long-press anywhere on tile to drag
             :on-touch-start
             (fn [_e]
               (touch-drag/start-pending!
                 (:id node) (:entity-id node) _e
                 (fn [] (reset! dragging true))))
             :on-touch-end
             (fn [_e]
               (touch-drag/cancel-pending!)
               (when (touch-drag/dragging?)
                 (reset! dragging false)
                 (when-let [drop-info (touch-drag/end-drag!)]
                   (let [{:keys [target-tile-id half source-entity-id source-tile-id]} drop-info
                         direction (half->direction half)]
                     (when (and target-tile-id
                                (not= source-tile-id target-tile-id)
                                (not (noop-rearrange? source-tile-id half @ctx-ref)))
                       (@split-ref target-tile-id source-entity-id direction :tile half))))))}

            ;; Header — mouse drag handle + name + close
            [:div.iris-entity-tile-header
             {:draggable true
              :on-double-click
              (fn [_e]
                (if (= @fullscreen-tile (:id node))
                  (reset! fullscreen-tile nil)
                  (reset! fullscreen-tile (:id node))))
              :on-drag-start
              (fn [e]
                (.setData (.-dataTransfer e) "text/plain"
                          (js/JSON.stringify #js {:tileId (:id node)
                                                   :entityId (:entity-id node)
                                                   :source "tile"}))
                (set! (.-effectAllowed (.-dataTransfer e)) "all")
                (reset! drag-source-tile (:id node))
                (reset! dragging true))
              :on-drag-end
              (fn [_e]
                (reset! dragging false)
                (reset! drag-source-tile nil))}
             [:span.iris-entity-tile-header-name entity-name]
             [:button.iris-entity-tile-header-close
              {:on-click (fn [e]
                           (.stopPropagation e)
                           (when @close-ref
                             (@close-ref (:entity-id node))))}
              "\u00d7"]]

            ;; Entity content
            [:div.iris-entity-tile-content
             (when (and entity render-entity-tile)
               [:> render-entity-tile entity])]

            ;; Drop indicator overlay
            [drop-indicator @closest-edge @drag-over]]))})))
