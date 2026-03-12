(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for React.

   Exports a single React component: `IrisLayout`

   See the `IrisLayout` def for full props documentation."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.components.entity-tile-group :as entity-tile-group]
            [iris-layout.components.entity-tile :as entity-tile]
            [iris-layout.components.touch-drag :as touch-drag]))

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
   Top-level keys stay as strings (entity IDs), nested keys are keywordized."
  [js-obj]
  (when js-obj
    (let [obj (js->clj js-obj)]
      (into {}
            (map (fn [[k v]]
                   [k (if (map? v)
                        (into {} (map (fn [[k2 v2]] [(keyword k2) v2]) v))
                        v)]))
            obj))))


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
   :on-active-entity-change - fn(entity-id) for focus changes"
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
    (fn [{:keys [layout entities render-entity-tile active-entity on-layout-change] :as props}]
      (reset! props-ref props)
      [:div.iris-body-stage
       [entity-tile-group/entity-tile-group
        layout handle-split handle-close handle-ratio active-entity entities render-entity-tile]])))


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
  [k workspace active? zoomed? props]
  (let [{:keys [entities render-entity-tile active-entity
                on-workspaces-change on-active-position-change on-entity-close
                workspaces]} props
        [x y] (mapv js/parseInt (.split k ","))]
    [:div {:class (str "iris-grid-cell"
                       (when active? " iris-grid-cell-active"))
           :data-position k
           :on-click (when (and zoomed? (not active?))
                       (fn [_]
                         (when on-active-position-change
                           (on-active-position-change [x y]))))}
     (if (:layout workspace)
       [body-stage-component
        {:layout (:layout workspace)
         :entities entities
         :render-entity-tile render-entity-tile
         :active-entity (when active? active-entity)
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
  [cols rows workspaces active-position zoomed? props]
  (let [active-key (pos-key active-position)]
    (for [y (range rows)]
      (for [x (range cols)]
        (let [k (pos-key [x y])
              workspace (get workspaces k)]
          ^{:key k}
          [grid-cell k workspace (= k active-key) zoomed? props])))))

(def ^:private grid-gap 16)

(defn- camera-style
  "Compute the CSS transform style for the grid canvas camera.
   Uses calc() to account for the gap between grid cells."
  [cols rows active-position zoomed?]
  (let [[ax ay] active-position
        scale (if zoomed? (/ 1 (max cols rows)) 1)
        ;; Each cell is (100% - (n-1)*gap) / n wide, plus gap between cells.
        ;; Offset for position p = p * (100% / n)  but we also shift by p * gap / n
        ;; to center each cell. Simplest: calc(-ax * (100% + gap) / cols)
        tx (if zoomed?
             "0px"
             (str "calc(" ax " * ((-100% - " grid-gap "px) / " cols "))"))
        ty (if zoomed?
             "0px"
             (str "calc(" ay " * ((-100% - " grid-gap "px) / " rows "))"))]
    {:width  (str "calc(" cols " * 100% + " (* (dec cols) grid-gap) "px)")
     :height (str "calc(" rows " * 100% + " (* (dec rows) grid-gap) "px)")
     :grid-template-columns (str "repeat(" cols ", 1fr)")
     :grid-template-rows    (str "repeat(" rows ", 1fr)")
     :transform (str "translate(" tx ", " ty ") scale(" scale ")")
     :transform-origin "0 0"}))

(defn grid-component
  "Grid — a 2D grid of workspaces with camera-based navigation.

   Workspaces are laid out in a real CSS grid. Navigation moves a camera
   (CSS translate) to show the active workspace. Holding Alt zooms out
   to show all workspaces at once."
  [_]
  (let [zoomed-out? (r/atom false)
        props-ref (atom nil)
        nav-drag-edge (r/atom nil)
        handle-nav (fn [dir] (handle-grid-nav dir props-ref))
        keydown-handler (fn [e]
                          (when (and (= (.-key e) "Alt") (not (.-repeat e)))
                            (reset! zoomed-out? true))
                          (when (.-altKey e)
                            (let [dir (case (.-key e)
                                        "ArrowLeft"  :left
                                        "ArrowRight" :right
                                        "ArrowUp"    :up
                                        "ArrowDown"  :down
                                        nil)]
                              (when dir
                                (.preventDefault e)
                                ;; Exit zoom-out first, then navigate (so slide is visible)
                                (reset! zoomed-out? false)
                                (handle-nav dir)))))
        keyup-handler (fn [e]
                        (when (= (.-key e) "Alt")
                          (reset! zoomed-out? false)))]
    (.addEventListener js/document "keydown" keydown-handler)
    (.addEventListener js/document "keyup" keyup-handler)
    ;; Watch touch-drag nav-edge-target for highlight + navigation
    (add-watch touch-drag/nav-edge-target ::grid-nav-highlight
      (fn [_ _ old-dir new-dir]
        (reset! nav-drag-edge new-dir)
        ;; Navigate when touch enters a nav edge
        (when (and new-dir (not= old-dir new-dir))
          (handle-grid-nav new-dir props-ref))))
    (fn [{:keys [workspaces active-position] :as props}]
      (reset! props-ref props)
      (let [[cols rows] (grid-dimensions workspaces)
            zoomed? @zoomed-out?
            dragging? (or @entity-tile/drag-source-tile @touch-drag/touch-state)
            vis? (fn [dir] (or dragging? (can-navigate? dir workspaces active-position)))]
        [:div {:class (str "iris-grid-viewport"
                          (when zoomed? " iris-grid-zoomed"))}
         (nav-edge-hiccup "iris-nav-left" (vis? :left) #(handle-nav :left) :left props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-right" (vis? :right) #(handle-nav :right) :right props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-top" (vis? :up) #(handle-nav :up) :up props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-bottom" (vis? :down) #(handle-nav :down) :down props-ref nav-drag-edge)
         [:div.iris-grid-center
          [:div.iris-grid-canvas
           {:style (camera-style cols rows active-position zoomed?)}
           (grid-canvas cols rows workspaces active-position zoomed? props)]]]))))

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
           onWorkspacesChange onActivePositionChange onActiveEntityChange onEntityClose]}]
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
    :on-entity-close onEntityClose}])

;; ============================================================
;; Exported React component
;; ============================================================

(def IrisLayout
  "A 2D tiling workspace grid for React.

   Renders an infinite grid of workspaces navigated by arrow clicks or
   Alt+Arrow keys. Each workspace contains a resizable, drag-and-drop
   tile layout. Hold Alt to zoom out and see the full grid overview.

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
