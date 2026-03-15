(ns iris-layout.components.entity-tile
  "Entity tile component — renders a single entity inside the Body layout."
  (:require [reagent.core :as r]
            [react-dom :as react-dom]
            [iris-layout.components.touch-drag :as touch-drag]))

(def preset-colors
  "Default color palette available in the tile color picker."
  ["#6366f1" "#8b5cf6" "#ec4899" "#f43f5e" "#f97316"
   "#eab308" "#22c55e" "#14b8a6" "#06b6d4" "#3b82f6" "#8b949e"])

(defn color-picker-popover
  "Color swatch popover for changing tile color. Rendered as a portal to escape overflow:hidden.
   Registers a document click handler on mount to close itself; removes it on unmount."
  [entity-id on-color-change show? anchor-rect active-color]
  (r/with-let [close-handler (atom nil)
               _ (let [handler (fn [_] (reset! show? false))]
                   (reset! close-handler handler)
                   (js/setTimeout #(.addEventListener js/document "click" handler) 0))]
    (let [colors (if (and active-color (not (some #{active-color} preset-colors)))
                   (cons active-color preset-colors)
                   preset-colors)]
      (react-dom/createPortal
        (r/as-element
          [:div.iris-color-picker-popover
           {:style {:top (+ (:bottom anchor-rect) 6) :left (:left anchor-rect)}
            :on-click (fn [e] (.stopPropagation e))}
           (doall
             (for [color colors]
               ^{:key color}
               [:div {:class (str "iris-color-swatch"
                                  (when (= color active-color) " iris-color-swatch-active"))
                      :style {:background color}
                      :on-click (fn [e]
                                  (.stopPropagation e)
                                  (when on-color-change (on-color-change entity-id color))
                                  (reset! show? false))}]))])
        js/document.body))
    (finally
      (when @close-handler
        (.removeEventListener js/document "click" @close-handler)))))

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
  "Map a :top/:bottom/:left/:right half keyword to a split direction string."
  [half]
  (if (or (= half :left) (= half :right)) "horizontal" "vertical"))

(def direction-labels
  "Human-readable labels for each drop half, shown in the drop indicator."
  {:top "Split above" :bottom "Split below"
   :left "Split left" :right "Split right"})

(defn drop-indicator
  "Renders the visual overlay showing where a dragged tile will land."
  [half visible?]
  (when visible?
    [:div.iris-drop-indicator
     [:div {:class (str "iris-drop-highlight iris-drop-" (name half))}]
     [:div.iris-drop-label-container
      [:span.iris-drop-label (get direction-labels half "Split")]]]))

;; Global drag/fullscreen state shared across all tile instances.
(defonce drag-source-tile
  "Tile ID of the tile currently being dragged."
  (atom nil))

(defonce drag-source-entity
  "Entity ID of the entity currently being dragged."
  (atom nil))

(defonce fullscreen-tile
  "Tile ID of the tile currently displayed fullscreen, or nil."
  (r/atom nil))

(defonce fullscreen-rect
  "Bounding rect captured before a tile enters fullscreen, used for FLIP animation."
  (atom nil))

(defn clear-drop-state!
  "Reset drag-over and closest-edge atoms when a drag leaves or completes."
  [drag-over closest-edge]
  (reset! drag-over false)
  (reset! closest-edge nil))

(defn handle-drag-over
  "Update drop indicator position while a foreign tile is dragged over this tile."
  [e dragging drag-over closest-edge tile-ref]
  (.preventDefault e)
  (if @dragging
    (clear-drop-state! drag-over closest-edge)
    (when-let [el @tile-ref]
      (reset! closest-edge (calculate-half e el))
      (reset! drag-over true))))

(defn handle-drop
  "Handle a native drag-and-drop drop event; triggers a split if the source is a foreign tile."
  [e node tile-ref split-ref drag-over closest-edge]
  (.preventDefault e)
  (.stopPropagation e)
  (when-let [el @tile-ref]
    (let [half      (calculate-half e el)
          direction (half->direction half)
          raw       (.getData (.-dataTransfer e) "text/plain")]
      (try
        (let [data (js/JSON.parse raw)]
          (when (and (= (.-source data) "tile") (.-entityId data)
                     (not= (.-tileId data) (:id node)))
            (@split-ref (:id node) (.-entityId data) direction half (.-tileId data))))
        (catch :default _ nil))))
  (clear-drop-state! drag-over closest-edge))

(defn handle-touch-end
  "Finalise or cancel an in-progress touch drag when the finger is lifted."
  [dragging]
  (touch-drag/cancel-touch!)
  (when (touch-drag/dragging?) (touch-drag/end-drag!))
  (reset! dragging false))

(defn handle-drag-start
  "Populate the dataTransfer payload and mark this tile as the drag source."
  [e node dragging]
  (.setData (.-dataTransfer e) "text/plain"
            (js/JSON.stringify #js {:tileId (:id node)
                                    :entityId (:entity-id node)
                                    :source "tile"}))
  (set! (.-effectAllowed (.-dataTransfer e)) "all")
  (reset! drag-source-tile (:id node))
  (reset! drag-source-entity (:entity-id node))
  (reset! dragging true))

(defn handle-drag-end
  "Clear drag-source globals when the drag gesture ends."
  [dragging]
  (reset! dragging false)
  (reset! drag-source-tile nil)
  (reset! drag-source-entity nil))

(defn make-tile-class
  "Build the CSS class string for the root tile div based on current state."
  [focused? drag-over dragging is-fullscreen?]
  (str "iris-entity-tile"
       (when focused? " iris-entity-tile-focused")
       (when (not focused?) " iris-entity-tile-unfocused")
       (when drag-over " iris-drag-over")
       (when dragging " iris-dragging")
       (when is-fullscreen? " iris-entity-tile-fullscreen")))

(defn flip-ref-callback
  "Returns a React ref callback that kicks off a FLIP animation when the tile enters fullscreen."
  [tile-ref is-fullscreen?]
  (fn [el]
    (reset! tile-ref el)
    (when (and el is-fullscreen? @fullscreen-rect)
      (let [{:keys [top left width height]} @fullscreen-rect
            sx (/ width (.-innerWidth js/window))
            sy (/ height (.-innerHeight js/window))]
        (set! (.-transform (.-style el))
              (str "translate(" left "px, " top "px) scale(" sx ", " sy ")"))
        (js/requestAnimationFrame
          (fn []
            (js/requestAnimationFrame
              (fn []
                (set! (.-transform (.-style el)) "none")
                (reset! fullscreen-rect nil)))))))))

(defn toggle-fullscreen!
  "Toggle fullscreen state for a tile, capturing the pre-fullscreen rect for FLIP animation."
  [tile-ref node]
  (if (= @fullscreen-tile (:id node))
    (do (reset! fullscreen-rect nil)
        (reset! fullscreen-tile nil))
    (do (when-let [el @tile-ref]
          (let [rect (.getBoundingClientRect el)]
            (reset! fullscreen-rect {:top    (.-top rect) :left   (.-left rect)
                                     :width  (.-width rect) :height (.-height rect)})))
        (reset! fullscreen-tile (:id node)))))

(defn tile-header
  "Renders the draggable header bar with the color dot, entity name, and close button."
  [node entity-name tile-color dragging close-ref
   color-picker-open? color-picker-rect color-change-ref tile-ref]
  [:div.iris-entity-tile-header
   {:draggable     true
    :on-double-click (fn [_] (toggle-fullscreen! tile-ref node))
    :on-drag-start   #(handle-drag-start % node dragging)
    :on-drag-end     (fn [_] (handle-drag-end dragging))
    :on-touch-start  (fn [e] (touch-drag/start-touch! (:id node) (:entity-id node) e))
    :on-touch-end    (fn [_e] (handle-touch-end dragging))}
   [:div {:style {:flex-shrink 0}}
    [:div.iris-entity-tile-header-dot
     {:style    {:background tile-color}
      :on-click (fn [e]
                  (.stopPropagation e)
                  (let [rect (.getBoundingClientRect (.-currentTarget e))]
                    (reset! color-picker-rect {:top    (.-top rect)  :left   (.-left rect)
                                               :bottom (.-bottom rect) :width (.-width rect)}))
                  (swap! color-picker-open? not))}]
    (when @color-picker-open?
      [color-picker-popover (:entity-id node) @color-change-ref
       color-picker-open? @color-picker-rect tile-color])]
   [:span.iris-entity-tile-header-name entity-name]
   [:button.iris-entity-tile-header-close
    {:on-click (fn [e]
                 (.stopPropagation e)
                 (when @close-ref (@close-ref (:id node))))}
    "\u00d7"]])

(defn make-hover-watch-fn
  "Returns a watch function that updates drag-over state when a touch drag hovers this tile."
  [node drag-over closest-edge]
  (fn [_ _ _ new-target]
    (if (and new-target (= (:tile-id new-target) (:id node)))
      (let [source-id (:tile-id @touch-drag/touch-state)]
        (if (= source-id (:id node))
          (clear-drop-state! drag-over closest-edge)
          (do (reset! closest-edge (:half new-target))
              (reset! drag-over true))))
      (when @drag-over
        (clear-drop-state! drag-over closest-edge)))))

(defn make-drop-watch-fn
  "Returns a watch function that triggers a split when a touch drag is dropped on this tile."
  [node drag-over closest-edge split-ref]
  (fn [_ _ _ drop-info]
    (when (and drop-info (= (:target-tile-id drop-info) (:id node)))
      (let [{:keys [source-tile-id source-entity-id half]} drop-info]
        (when (not= source-tile-id (:id node))
          (@split-ref (:id node) source-entity-id (half->direction half) half source-tile-id)))
      (reset! touch-drag/drop-result nil)
      (clear-drop-state! drag-over closest-edge))))

(defn entity-tile-component
  "Renders a single entity tile with drag-and-drop, touch drag, fullscreen, and color-picker support.
   Mounts touch-drag watches on creation and removes them on unmount."
  [node on-split on-close focused? entities render-entity-tile _parent-ctx on-active-entity-change on-entity-color-change]
  (r/with-let [drag-over              (r/atom false)
               closest-edge           (r/atom nil)
               dragging               (r/atom false)
               color-picker-open?     (r/atom false)
               color-picker-rect      (r/atom nil)
               split-ref              (atom on-split)
               close-ref              (atom on-close)
               active-entity-chg-ref  (atom on-active-entity-change)
               color-change-ref       (atom on-entity-color-change)
               tile-ref               (atom nil)
               touch-watch-key        (str "tile-" (:id node))
               drop-watch-key         (str "tile-drop-" (:id node))
               _ (add-watch touch-drag/hover-target touch-watch-key
                   (make-hover-watch-fn node drag-over closest-edge))
               _ (add-watch touch-drag/drop-result drop-watch-key
                   (make-drop-watch-fn node drag-over closest-edge split-ref))]
    ;; Sync callback refs on every render so watches always call the latest fns.
    (reset! split-ref on-split)
    (reset! close-ref on-close)
    (reset! active-entity-chg-ref on-active-entity-change)
    (reset! color-change-ref on-entity-color-change)
    (let [entity        (get entities (keyword (:entity-id node)))
          tile-color    (or (:color entity) "#6366f1")
          is-fullscreen? (= @fullscreen-tile (:id node))]
      [:div
       {:ref           (flip-ref-callback tile-ref is-fullscreen?)
        :data-tile-id  (:id node)
        :class         (make-tile-class focused? @drag-over @dragging is-fullscreen?)
        :style         (cond-> {:flex 1} (:color entity) (assoc "--iris-tile-color" (:color entity)))
        :on-mouse-enter (fn [_]
                          (when (and (not focused?) @active-entity-chg-ref)
                            (@active-entity-chg-ref (:entity-id node))))
        :on-drag-over  #(handle-drag-over % dragging drag-over closest-edge tile-ref)
        :on-drag-enter (fn [e] (.preventDefault e))
        :on-drag-leave (fn [e]
                         (when-not (.contains (.-currentTarget e) (.-relatedTarget e))
                           (clear-drop-state! drag-over closest-edge)))
        :on-drop       #(handle-drop % node tile-ref split-ref drag-over closest-edge)}
       [tile-header node (:name entity) tile-color dragging close-ref
        color-picker-open? color-picker-rect color-change-ref tile-ref]
       [:div.iris-entity-tile-content
        (when render-entity-tile
          [render-entity-tile (or entity {:id (:entity-id node)})])]
       [drop-indicator @closest-edge @drag-over]])
    (finally
      (remove-watch touch-drag/hover-target touch-watch-key)
      (remove-watch touch-drag/drop-result drop-watch-key))))
