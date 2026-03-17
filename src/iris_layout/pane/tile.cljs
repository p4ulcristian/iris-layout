(ns iris-layout.pane.tile
  "Tile component — renders a single entity inside the pane layout."
  (:require [reagent.core :as r]
            [react-dom :as react-dom]
            ["@dnd-kit/core" :refer [useDraggable useDroppable]]))

;; Drag ghost state: entity info + mouse position + dimensions, rendered via portal.
(def drag-ghost (r/atom nil))

(defn on-mousemove [e]
  (when @drag-ghost
    (swap! drag-ghost assoc :x (.-clientX e) :y (.-clientY e))))

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
            :on-pointer-down (fn [e] (.stopPropagation e))
            :on-click (fn [e] (.stopPropagation e))}
           (doall
             (for [color colors]
               ^{:key color}
               [:div {:class (str "iris-color-swatch"
                                  (when (= color active-color) " iris-color-swatch-active"))
                      :style {:background color}
                      :on-pointer-down (fn [e] (.stopPropagation e))
                      :on-click (fn [e]
                                  (.stopPropagation e)
                                  (when on-color-change (on-color-change entity-id color))
                                  (reset! show? false))}]))])
        js/document.body))
    (finally
      (when @close-handler
        (.removeEventListener js/document "click" @close-handler)))))

(defn calculate-half-from-pointer
  "Determine which half of a tile the pointer is in, using pointer coords and the tile's bounding rect."
  [pointer-x pointer-y over-rect]
  (let [cx (+ (.-left over-rect) (/ (.-width over-rect) 2))
        cy (+ (.-top over-rect) (/ (.-height over-rect) 2))
        dx (- pointer-x cx)
        dy (- pointer-y cy)]
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

;; Global drag state shared across all tile instances.
(defonce fullscreen-tile (r/atom nil))
(defonce fullscreen-rect (atom nil))

;; Set by DndContext.onDragStart/End in core.cljs.
(def dragging-tile (r/atom nil))

;; Entity info for the drag overlay: {:entity-id ... :name ... :color ...}
(def dragging-entity (r/atom nil))

;; Set by DndContext.onDragMove in core.cljs: {:tile-id "..." :half :left}
(def drop-target (r/atom nil))

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

(defn make-tile-class
  "Build the CSS class string for the root tile div based on current state."
  [focused? drag-over? dragging? is-fullscreen?]
  (str "iris-entity-tile"
       (when focused? " iris-entity-tile-focused")
       (when (not focused?) " iris-entity-tile-unfocused")
       (when drag-over? " iris-drag-over")
       (when dragging? " iris-dragging")
       (when is-fullscreen? " iris-entity-tile-fullscreen")))

(defn tile-header-fc
  "Draggable tile header — uses useDraggable to initiate a dnd-kit drag gesture."
  [{:keys [node entity-name tile-color close-ref
           color-picker-open? color-picker-rect color-change-ref tile-ref]}]
  (let [result    (useDraggable #js {:id   (:id node)
                                     :data #js {:tile_id   (:id node)
                                                :entity_id (:entity-id node)}})
        set-ref   (.-setNodeRef result)
        listeners (js->clj (.-listeners result) :keywordize-keys true)
        attrs     (js->clj (.-attributes result) :keywordize-keys true)]
    [:div.iris-entity-tile-header
     (merge {:ref             (fn [el] (reset! tile-ref el) (set-ref el))
             :on-double-click (fn [_] (toggle-fullscreen! tile-ref node))}
            listeners attrs)
     [:div {:style {:flex-shrink 0}}
      [:div.iris-entity-tile-header-dot
       {:style    {:background tile-color}
        :on-pointer-down (fn [e] (.stopPropagation e))
        :on-pointer-up (fn [e]
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
      {:on-pointer-down (fn [e] (.stopPropagation e))
       :on-click (fn [e]
                   (.stopPropagation e)
                   (when @close-ref (@close-ref (:id node))))}
      "\u00d7"]]))

(defn entity-tile-component
  "Renders a single entity tile with drag-and-drop, fullscreen, and color-picker support."
  [node _on-split on-close focused? entities render-entity-tile _parent-ctx on-active-entity-change on-entity-color-change]
  (r/with-let [color-picker-open?    (r/atom false)
               color-picker-rect     (r/atom nil)
               close-ref             (atom on-close)
               active-entity-chg-ref (atom on-active-entity-change)
               color-change-ref      (atom on-entity-color-change)
               tile-ref              (atom nil)]
    (reset! close-ref on-close)
    (reset! active-entity-chg-ref on-active-entity-change)
    (reset! color-change-ref on-entity-color-change)
    (let [drop-result  (useDroppable #js {:id (str "tile:" (:id node))})
          set-drop-ref (.-setNodeRef drop-result)
          entity       (get entities (keyword (:entity-id node)))
          tile-color   (or (:color entity) "#6366f1")
          is-fullscreen? (= @fullscreen-tile (:id node))
          dt           @drop-target
          drag-over?   (= (:tile-id dt) (:id node))
          dragging?    (= @dragging-tile (:id node))]
      [:div
       {:ref            (fn [el]
                          ((flip-ref-callback tile-ref is-fullscreen?) el)
                          (set-drop-ref el))
        :data-tile-id   (:id node)
        :class          (make-tile-class focused? drag-over? dragging? is-fullscreen?)
        :style          (cond-> {:flex 1} (:color entity) (assoc "--iris-tile-color" (:color entity)))
        :on-mouse-enter (fn [_]
                          (when @active-entity-chg-ref
                            (@active-entity-chg-ref (:entity-id node))))}
       [tile-header-fc {:node               node
                             :entity-name        (:name entity)
                             :tile-color         tile-color
                             :close-ref          close-ref
                             :color-picker-open? color-picker-open?
                             :color-picker-rect  color-picker-rect
                             :color-change-ref   color-change-ref
                             :tile-ref           tile-ref}]
       [:div.iris-entity-tile-content
        (when render-entity-tile
          [render-entity-tile (or entity {:id (:entity-id node)})])]
       [drop-indicator (when drag-over? (:half dt)) drag-over?]])))

(defn drag-ghost-portal []
  (when-let [g @drag-ghost]
    (react-dom/createPortal
      (r/as-element
        [:div.iris-entity-tile
         {:style {:position       "fixed"
                  :left           (- (:x g) (or (:offset-x g) 0))
                  :top            (- (:y g) (or (:offset-y g) 0))
                  :width          (:width g)
                  :height         (:height g)
                  :z-index        10000
                  :pointer-events "none"
                  :opacity        0.85
                  "--iris-tile-color" (or (:color g) "#6366f1")}}
         [:div.iris-entity-tile-header
          [:div {:style {:flex-shrink 0}}
           [:div.iris-entity-tile-header-dot
            {:style {:background (or (:color g) "#6366f1")}}]]
          [:span.iris-entity-tile-header-name (:name g)]]])
      js/document.body)))
