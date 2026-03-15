(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for React.

   Exports a single React component: `IrisLayout`

   See the `IrisLayout` def for full props documentation."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.components.entity-tile-group :as entity-tile-group]
            [iris-layout.components.entity-tile :as entity-tile]
            [iris-layout.components.touch-drag :as touch-drag]
            [iris-layout.components.command-center :as command-center]))

;; ============================================================
;; JS <-> CLJS boundary conversion
;; ============================================================

(defn- generate-id []
  (str "iris-" (random-uuid)))

(defn js->layout
  "Convert a JS layout object to a CLJS layout map.
   Handles recursive conversion of the tree structure.
   JS keys: type, id, direction, ratio, children, entityId
   CLJS keys: :type, :id, :direction, :ratio, :children, :entity-id"
  [js-obj]
  (when js-obj
    (let [obj (js->clj js-obj)]
      (cond-> {:type (keyword (get obj "type"))
               :id (get obj "id")}
        (get obj "direction") (assoc :direction (keyword (get obj "direction")))
        (get obj "ratio")     (assoc :ratio (get obj "ratio"))
        (get obj "children")  (assoc :children (mapv js->layout (get obj "children")))
        (or (get obj "entityId")
            (get obj "entity-id")) (assoc :entity-id (or (get obj "entityId")
                                                          (get obj "entity-id")))))))

(defn layout->js
  "Convert a CLJS layout map to a JS object.
   Inverse of js->layout."
  [layout]
  (when layout
    (clj->js
      (cond-> {"type" (name (:type layout))
               "id" (:id layout)}
        (:direction layout)  (assoc "direction" (name (:direction layout)))
        (:ratio layout)      (assoc "ratio" (:ratio layout))
        (:children layout)   (assoc "children" (mapv layout->js (:children layout)))
        (:entity-id layout)  (assoc "entityId" (:entity-id layout))))))

(defn js->entities
  "Convert a JS entities object to a CLJS map.
   Top-level keys stay as strings (entity IDs), nested keys are keywordized.
   React elements (icon, etc.) are preserved as-is."
  [js-obj]
  (when js-obj
    (let [keys (js/Object.keys js-obj)]
      (into {}
            (map (fn [k]
                   (let [v (unchecked-get js-obj k)]
                     [k (if (object? v)
                          (let [vkeys (js/Object.keys v)]
                            (into {}
                                  (map (fn [k2]
                                         [(keyword k2) (unchecked-get v k2)]))
                                  vkeys))
                          v)])))
            keys))))


;; ============================================================
;; Fullscreen overlay
;; ============================================================

(defn- fullscreen-overlay
  [fs-node fs-entity render-entity-tile on-entity-color-change]
  (let [color-picker-open? (r/atom false)
        color-picker-rect (r/atom nil)]
    (fn [fs-node fs-entity render-entity-tile on-entity-color-change]
      (let [fs-color (or (:color fs-entity) "#6366f1")]
        [:div.iris-fullscreen-overlay
         {:style (when (:color fs-entity) {"--iris-tile-color" (:color fs-entity)})}
         [:div.iris-fullscreen-overlay-header
          {:on-double-click (fn [_] (reset! entity-tile/fullscreen-tile nil))}
          [:div {:style {:flex-shrink 0}}
           [:div.iris-entity-tile-header-dot
            {:style {:background fs-color}
             :on-click (fn [e]
                         (.stopPropagation e)
                         (let [rect (.getBoundingClientRect (.-currentTarget e))]
                           (reset! color-picker-rect
                                   {:top (.-top rect) :left (.-left rect)
                                    :bottom (.-bottom rect) :width (.-width rect)}))
                         (swap! color-picker-open? not))}]
           (when @color-picker-open?
             [entity-tile/color-picker-popover (:entity-id fs-node) on-entity-color-change color-picker-open? @color-picker-rect fs-color])]
          [:span.iris-entity-tile-header-name
           (or (:name fs-entity) (:entity-id fs-node))]
          [:button.iris-entity-tile-header-close
           {:on-click (fn [e]
                        (.stopPropagation e)
                        (reset! entity-tile/fullscreen-tile nil))}
           "\u00d7"]]
         [:div.iris-entity-tile-content
          (when (and fs-entity render-entity-tile)
            [:> render-entity-tile fs-entity])]]))))

;; ============================================================
;; Body stage — single workspace layout renderer
;; ============================================================

(defn- body-stage-component
  "A single layout stage within the Body.

   Manages drag-drop split operations:
   - Sidebar drags: adds entity to layout via split
   - Tile rearranges: removes entity from source, splits at target

   Props:
   :layout             - layout tree (CLJS map)
   :entities           - entity data map
   :render-entity-tile - React component for tile content
   :active-entity      - focused entity ID
   :on-layout-change   - fn(new-layout) for layout mutations
   :on-active-entity-change - fn(entity-id) for focus changes
   :on-entity-color-change  - fn(entity-id, color) for tile color changes"
  [_]
  (let [props-ref (atom nil)
        handle-split (fn [tile-id entity-id split-direction source-type half]
                       (let [{:keys [layout on-layout-change]} @props-ref
                             before? (or (= half :left) (= half :top))
                             base-layout (if (= source-type :tile)
                                           (or (layout/remove-entity-from-layout layout entity-id)
                                               layout)
                                           layout)
                             target-after (layout/find-tile base-layout tile-id)
                             new-tile-id (generate-id)
                             split-id (generate-id)
                             new-layout (when target-after
                                          (layout/split-tile
                                            base-layout tile-id split-direction
                                            entity-id new-tile-id split-id before?))]
                         (when (and new-layout on-layout-change)
                           (on-layout-change new-layout))))
        handle-close (fn [entity-id]
                       (let [{:keys [layout on-layout-change on-entity-close]} @props-ref
                             new-layout (layout/remove-entity-from-layout layout entity-id)]
                         (when on-layout-change
                           (on-layout-change new-layout))
                         (when on-entity-close
                           (on-entity-close entity-id))))
        handle-ratio (fn [split-id new-ratio]
                       (let [{:keys [layout on-layout-change]} @props-ref
                             new-layout (layout/update-split-ratio layout split-id new-ratio)]
                         (when on-layout-change
                           (on-layout-change new-layout))))]
    (fn [{:keys [layout entities render-entity-tile active-entity on-layout-change on-active-entity-change on-entity-color-change] :as props}]
      (reset! props-ref props)
      [:div.iris-body-stage
       [entity-tile-group/entity-tile-group
        layout handle-split handle-close handle-ratio active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]])))


;; ============================================================
;; Grid — 2D workspace grid with camera-based navigation
;; ============================================================

(defn- pos-key
  "Convert [x y] to string key 'x,y'."
  [[x y]]
  (str x "," y))

(defn- direction->delta
  "Map a direction keyword to [dx dy]."
  [dir]
  (case dir
    :left  [-1 0]
    :right [1 0]
    :up    [0 -1]
    :down  [0 1]))

(defn- grid-dimensions
  "Compute grid bounds from workspace keys. Returns [cols rows]."
  [workspaces]
  (if (empty? workspaces)
    [1 1]
    (let [positions (map (fn [k]
                           (let [parts (.split k ",")]
                             [(js/parseInt (aget parts 0))
                              (js/parseInt (aget parts 1))]))
                         (keys workspaces))
          max-x (apply max (map first positions))
          max-y (apply max (map second positions))]
      [(inc max-x) (inc max-y)])))

(defn- can-navigate?
  "Check if navigation in a direction is allowed."
  [dir workspaces active-position]
  (let [[dx dy] (direction->delta dir)
        [x y] active-position
        new-x (+ x dx)
        new-y (+ y dy)
        new-key (pos-key [new-x new-y])
        active-key (pos-key active-position)
        current-empty? (nil? (:layout (get workspaces active-key)))
        target-exists? (contains? workspaces new-key)
        target-has-layout? (and target-exists?
                                (:layout (get workspaces new-key)))]
    (and (>= new-x 0) (>= new-y 0)
         (not (and current-empty? (not target-has-layout?))))))

(defn- handle-grid-nav
  "Handle navigation in a direction. Creates empty workspace if needed."
  [dir props-ref]
  (let [{:keys [workspaces active-position
                on-workspaces-change on-active-position-change]} @props-ref]
    (when (can-navigate? dir workspaces active-position)
      (let [[dx dy] (direction->delta dir)
            [x y] active-position
            new-pos [(+ x dx) (+ y dy)]
            new-key (pos-key new-pos)
            target-exists? (contains? workspaces new-key)]
        (when (and (not target-exists?) on-workspaces-change)
          (on-workspaces-change (assoc workspaces new-key {:layout nil})))
        (when on-active-position-change
          (on-active-position-change new-pos))))))

(defn- nav-edge-hiccup
  "Return hiccup for a navigation edge button with half-circle indicator.
   During drag, hovering navigates to adjacent workspace (user keeps dragging to drop there)."
  [css-class visible? on-click direction props-ref nav-drag-edge]
  [:div {:class (str "iris-nav-edge " css-class
                     (when-not visible? " iris-nav-hidden")
                     (when (= @nav-drag-edge direction) " iris-nav-drag-over"))
         :on-click on-click
         :on-drag-over (fn [e] (.preventDefault e))
         :on-drag-enter (fn [e]
                          (.preventDefault e)
                          (reset! nav-drag-edge direction)
                          (handle-grid-nav direction props-ref))
         :on-drag-leave (fn [e]
                          (when-not (.contains (.-currentTarget e) (.-relatedTarget e))
                            (reset! nav-drag-edge nil)))
         :on-drop (fn [e]
                    (.preventDefault e)
                    (.stopPropagation e)
                    (reset! nav-drag-edge nil))}
   [:div.iris-nav-semicircle]])

(defn- update-workspaces-with-cleanup
  "Update a workspace's layout and remove dragged entity from all other workspaces."
  [workspaces target-key new-layout]
  (let [dragged-entity @entity-tile/drag-source-entity]
    (if dragged-entity
      (reduce-kv
        (fn [acc ws-key ws-data]
          (if (= ws-key target-key)
            (assoc acc ws-key {:layout new-layout})
            (let [cleaned (layout/remove-entity-from-layout
                            (:layout ws-data) dragged-entity)]
              (assoc acc ws-key {:layout cleaned}))))
        {} workspaces)
      (assoc workspaces target-key {:layout new-layout}))))

(defn- handle-empty-workspace-drop
  "Handle drop on an empty workspace — create a tile for the dropped entity."
  [e k workspaces on-workspaces-change]
  (.preventDefault e)
  (let [raw (.getData (.-dataTransfer e) "text/plain")]
    (try
      (let [data (js/JSON.parse raw)
            entity-id (.-entityId data)]
        (when entity-id
          (let [new-layout {:type :tile :id (generate-id) :entity-id entity-id}
                updated (update-workspaces-with-cleanup workspaces k new-layout)]
            (on-workspaces-change updated))))
      (catch :default _ nil))))

(defn- grid-cell
  "Render a single grid cell (workspace or empty placeholder)."
  [k workspace active? props]
  (let [{:keys [entities render-entity-tile active-entity
                on-workspaces-change on-entity-close
                on-active-entity-change on-entity-color-change workspaces]} props]
    [:div {:class (str "iris-grid-cell"
                       (when active? " iris-grid-cell-active"))
           :data-position k}
     (if (:layout workspace)
       [body-stage-component
        {:layout (:layout workspace)
         :entities entities
         :render-entity-tile render-entity-tile
         :active-entity (when active? active-entity)
         :on-active-entity-change on-active-entity-change
         :on-entity-color-change on-entity-color-change
         :on-layout-change
         (fn [new-layout]
           (when on-workspaces-change
             (on-workspaces-change
               (update-workspaces-with-cleanup workspaces k new-layout))))
         :on-entity-close on-entity-close}]
       [:div.iris-empty-workspace
        {:on-drag-over (fn [e] (.preventDefault e))
         :on-drop (fn [e]
                    (when on-workspaces-change
                      (handle-empty-workspace-drop e k workspaces on-workspaces-change)))}])]))

(defn- grid-canvas
  "Render the CSS grid canvas with all workspace cells."
  [cols rows workspaces active-position props]
  (let [active-key (pos-key active-position)]
    (for [y (range rows)]
      (for [x (range cols)]
        (let [k (pos-key [x y])
              workspace (get workspaces k)]
          ^{:key k}
          [grid-cell k workspace (= k active-key) props])))))

(def ^:private grid-gap 16)

(defn- camera-style
  "Compute the CSS transform style for the grid canvas camera.
   Uses calc() to account for the gap between grid cells."
  [cols rows active-position]
  (let [[ax ay] active-position
        tx (str "calc(" ax " * ((-100% - " grid-gap "px) / " cols "))")
        ty (str "calc(" ay " * ((-100% - " grid-gap "px) / " rows "))")]
    {:width  (str "calc(" cols " * 100% + " (* (dec cols) grid-gap) "px)")
     :height (str "calc(" rows " * 100% + " (* (dec rows) grid-gap) "px)")
     :grid-template-columns (str "repeat(" cols ", 1fr)")
     :grid-template-rows    (str "repeat(" rows ", 1fr)")
     :transform (str "translate(" tx ", " ty ")")
     :transform-origin "0 0"}))

(defn grid-component
  "Grid — a 2D grid of workspaces with camera-based navigation.

   Workspaces are laid out in a real CSS grid. Navigation moves a camera
   (CSS translate) to show the active workspace. Hold Alt to open the
   Command Center overlay."
  [_]
  (let [command-center? (r/atom false)
        props-ref (atom nil)
        nav-drag-edge (r/atom nil)
        touch-nav-happened? (atom false)
        handle-nav (fn [dir] (handle-grid-nav dir props-ref))
        keydown-handler (fn [e]
                          (when (and (= (.-key e) "Alt") (not (.-repeat e)))
                            (reset! command-center? true))
                          (when (.-altKey e)
                            (let [dir (case (.-key e)
                                        "ArrowLeft"  :left
                                        "ArrowRight" :right
                                        "ArrowUp"    :up
                                        "ArrowDown"  :down
                                        nil)]
                              (when dir
                                (.preventDefault e)
                                (reset! command-center? false)
                                (handle-nav dir)))))
        keyup-handler (fn [e]
                        (when (= (.-key e) "Alt")
                          (reset! command-center? false)))]
    (.addEventListener js/document "keydown" keydown-handler)
    (.addEventListener js/document "keyup" keyup-handler)
    ;; Watch touch-drag nav-edge-target for highlight + navigation
    (add-watch touch-drag/nav-edge-target ::grid-nav-highlight
      (fn [_ _ old-dir new-dir]
        (reset! nav-drag-edge new-dir)
        ;; Navigate when touch enters a nav edge
        (when (and new-dir (not= old-dir new-dir))
          (reset! touch-nav-happened? true)
          (handle-grid-nav new-dir props-ref))))
    ;; Watch touch-drag drop-result for drops with no target tile after workspace nav
    (add-watch touch-drag/drop-result ::grid-touch-drop
      (fn [_ _ _ drop-info]
        (when (and drop-info
                   (not (:target-tile-id drop-info))
                   @touch-nav-happened?)
          (let [{:keys [source-entity-id]} drop-info
                {:keys [workspaces active-position on-workspaces-change]} @props-ref
                active-key (pos-key active-position)]
            (when (and source-entity-id on-workspaces-change)
              (let [new-tile {:type :tile :id (generate-id) :entity-id source-entity-id}
                    existing-layout (:layout (get workspaces active-key))
                    new-layout (if existing-layout
                                 ;; Add alongside existing layout
                                 {:type :split :id (generate-id)
                                  :direction :horizontal :ratio 0.5
                                  :children [existing-layout new-tile]}
                                 new-tile)
                    updated (reduce-kv
                              (fn [acc ws-key ws-data]
                                (if (= ws-key active-key)
                                  (assoc acc ws-key {:layout new-layout})
                                  (let [cleaned (layout/remove-entity-from-layout
                                                  (:layout ws-data) source-entity-id)]
                                    (assoc acc ws-key {:layout cleaned}))))
                              {} workspaces)]
                (on-workspaces-change updated))))
          (reset! touch-drag/drop-result nil))
        (reset! touch-nav-happened? false)))
    (fn [{:keys [workspaces active-position entities render-entity-tile
                 on-active-position-change on-workspaces-change logo] :as props}]
      (reset! props-ref props)
      (let [[cols rows] (grid-dimensions workspaces)
            dragging? (or @entity-tile/drag-source-tile @touch-drag/touch-state)
            vis? (fn [dir] (or dragging? (can-navigate? dir workspaces active-position)))]
        [:div.iris-grid-viewport
         (nav-edge-hiccup "iris-nav-left" (vis? :left) #(handle-nav :left) :left props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-right" (vis? :right) #(handle-nav :right) :right props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-top" (vis? :up) #(handle-nav :up) :up props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-bottom" (vis? :down) #(handle-nav :down) :down props-ref nav-drag-edge)
         [:div.iris-grid-center
          [:div.iris-grid-canvas
           {:style (camera-style cols rows active-position)}
           (grid-canvas cols rows workspaces active-position props)]]
         ;; Quarter-circle trigger button (bottom-left)
         [:div.iris-command-center-trigger
          {:on-click (fn [_] (reset! command-center? true))}
          [:svg.iris-command-center-trigger-bg {:viewBox "0 0 48 48" :width "100%" :height "100%"}
           [:path {:d "M0 48 L0 0 A48 48 0 0 1 48 48 Z"
                   :fill "currentColor"}]]
          [:div.iris-command-center-trigger-logo
           (if logo
             logo
             [:svg {:viewBox "0 0 24 24" :width "20" :height "20"}
              [:circle {:cx 12 :cy 12 :r 7
                        :fill "none"
                        :stroke "currentColor"
                        :stroke-width 1.5}]
              [:circle {:cx 12 :cy 12 :r 2.5
                        :fill "currentColor"}]])]]
         (when @command-center?
           [command-center/command-center-overlay cols rows workspaces active-position
            entities render-entity-tile on-active-position-change
            on-workspaces-change command-center?])]))))

;; ============================================================
;; JS <-> CLJS: Grid conversions
;; ============================================================

(defn js->workspaces
  "Convert a JS workspaces object {\"x,y\": {layout: ...}} to CLJS map."
  [js-obj]
  (when js-obj
    (let [obj (js->clj js-obj)]
      (into {}
            (map (fn [[k v]]
                   [k {:layout (js->layout (get v "layout"))}]))
            obj))))

(defn workspaces->js
  "Convert CLJS workspaces map to JS object."
  [workspaces]
  (clj->js
    (into {}
          (map (fn [[k v]]
                 [k {"layout" (layout->js (:layout v))}]))
          workspaces)))

(defn- grid-wrapper
  "Wrapper that converts JS props to CLJS for grid-component."
  [{:keys [workspaces activePosition activeEntity entities renderEntityTile
           onWorkspacesChange onActivePositionChange onActiveEntityChange onEntityClose
           onEntityColorChange logo]}]
  [grid-component
   {:workspaces (js->workspaces workspaces)
    :active-position (js->clj activePosition)
    :active-entity activeEntity
    :entities (js->entities entities)
    :render-entity-tile renderEntityTile
    :on-workspaces-change (when onWorkspacesChange
                            (fn [new-workspaces]
                              (onWorkspacesChange (workspaces->js new-workspaces))))
    :on-active-position-change (when onActivePositionChange
                                 (fn [new-pos]
                                   (onActivePositionChange (clj->js new-pos))))
    :on-active-entity-change onActiveEntityChange
    :on-entity-close onEntityClose
    :on-entity-color-change onEntityColorChange
    :logo logo}])

;; ============================================================
;; Exported React component
;; ============================================================

(def IrisLayout
  "A 2D tiling workspace grid for React.

   Renders an infinite grid of workspaces navigated by arrow clicks or
   Alt+Arrow keys. Each workspace contains a resizable, drag-and-drop
   tile layout. Hold Alt to open the Command Center overlay.

   Props (camelCase JS object):

     workspaces              - Object keyed by 'x,y' position strings.
                               Each value: { layout: LayoutNode }
                               LayoutNode is either:
                                 { type: 'tile',  id, entityId }
                                 { type: 'split', id, direction: 'horizontal'|'vertical',
                                   ratio: 0-1, children: [LayoutNode, LayoutNode] }

     activePosition          - [x, y] array — which workspace is visible.

     activeEntity            - Entity ID string of the focused tile (or null).

     entities                - Object keyed by entity ID.
                               Each value: { id, name, ... } (your domain data).

     renderEntityTile        - React component rendered inside each tile.
                               Receives the entity object as props.

     onWorkspacesChange      - fn(workspaces) — called on layout mutations
                               (splits, closes, resizes, cross-workspace drags).

     onActivePositionChange  - fn([x, y]) — called when navigating between workspaces.

     onActiveEntityChange    - fn(entityId) — called when a tile gains focus.

     onEntityClose           - fn(entityId) — optional hook when a tile is closed.

   CSS: import '@p4ulcristian/iris-layout/styles.css'
   Customize gap: set --iris-grid-gap on a parent element."
  (r/reactify-component grid-wrapper))
