(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for React.

   Exports a single React component: `IrisLayout`

   See the `IrisLayout` def for full props documentation."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.components.entity-tile-group :as entity-tile-group]
            [iris-layout.components.entity-tile :as entity-tile]
            [iris-layout.components.touch-drag :as touch-drag]
            [iris-layout.components.command-center :as command-center]))

(r/set-default-compiler! (r/create-compiler {:function-components true}))


;; ============================================================
;; Body stage — single workspace layout renderer
;; ============================================================

(defn- handle-split
  "Insert a new tile into the layout by splitting an existing one.

   tile-id        - target tile to split
   entity-id      - entity to place in the new tile
   split-direction - \"horizontal\" or \"vertical\"
   half           - :left :right :top :bottom — which side of the target was dropped on
   source-tile-id - the tile being moved; removed from the layout before splitting"
  [layout on-layout-change tile-id entity-id split-direction half source-tile-id]
  (let [before? (or (= half :left) (= half :top))
        base-layout (or (layout/remove-tile-from-layout layout source-tile-id) layout)
        target (layout/find-tile base-layout tile-id)
        new-layout (when target
                     (layout/split-tile base-layout tile-id split-direction
                                        entity-id (util/generate-id) (util/generate-id) before?))]
    (when (and new-layout on-layout-change)
      (on-layout-change new-layout))))

(defn- handle-close
  "Remove a tile from the layout and notify the consumer.
   Calls on-layout-change with the pruned layout (nil if the last tile is closed).
   Calls on-entity-close with the entity-id of the closed tile."
  [layout on-layout-change on-entity-close tile-id]
  (let [tile (layout/find-tile layout tile-id)
        new-layout (layout/remove-tile-from-layout layout tile-id)]
    (when on-layout-change (on-layout-change new-layout))
    (when on-entity-close (on-entity-close (:entity-id tile)))))

(defn- handle-ratio
  "Update the split ratio of a split node.
   new-ratio is a float 0–1 representing the size of the first child."
  [layout on-layout-change split-id new-ratio]
  (when on-layout-change
    (on-layout-change (layout/update-split-ratio layout split-id new-ratio))))

(defn- body-stage-component
  "Renders a single workspace — a resizable, drag-drop tile layout.
   Wires up split, close, and resize handlers and delegates rendering
   to entity-tile-group."
  [{:keys [layout entities render-entity-tile active-entity
           on-layout-change on-active-entity-change on-entity-color-change on-entity-close]}]
  [:div.iris-body-stage
   [entity-tile-group/entity-tile-group
    layout
    (partial handle-split layout on-layout-change)
    (partial handle-close layout on-layout-change on-entity-close)
    (partial handle-ratio layout on-layout-change)
    active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]])


;; ============================================================
;; Grid — 2D workspace grid with camera-based navigation
;; ============================================================

(defn- direction->delta
  "Map a direction keyword to [dx dy]."
  [dir]
  (case dir
    :left  [-1 0]
    :right [1 0]
    :up    [0 -1]
    :down  [0 1]))

(defn- grid-dimensions
  "Compute grid bounds from workspace positions. Returns [cols rows]."
  [workspaces]
  (if (empty? workspaces)
    [1 1]
    [(inc (apply max (map :x (vals workspaces))))
     (inc (apply max (map :y (vals workspaces))))]))

(defn- can-navigate?
  "Check if navigation in a direction is allowed."
  [dir workspaces active-position]
  (let [[dx dy] (direction->delta dir)
        [x y] active-position
        new-x (+ x dx)
        new-y (+ y dy)
        [_ active-ws] (util/workspace-at workspaces x y)
        current-empty? (nil? (:layout active-ws))
        [_ target-ws] (util/workspace-at workspaces new-x new-y)
        target-has-layout? (boolean (and target-ws (:layout target-ws)))]
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
            new-x (+ x dx)
            new-y (+ y dy)
            new-pos [new-x new-y]]
        (when (and (not (util/workspace-at workspaces new-x new-y)) on-workspaces-change)
          (on-workspaces-change (assoc workspaces (util/generate-id) {:x new-x :y new-y :layout nil})))
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
  "Update a workspace's layout and remove dragged tile from all other workspaces."
  [workspaces target-id new-layout]
  (let [dragged-tile @entity-tile/drag-source-tile]
    (if dragged-tile
      (reduce-kv
        (fn [acc ws-id ws-data]
          (if (= ws-id target-id)
            (assoc acc ws-id (assoc ws-data :layout new-layout))
            (assoc acc ws-id (assoc ws-data :layout
                               (layout/remove-tile-from-layout (:layout ws-data) dragged-tile)))))
        {} workspaces)
      (update workspaces target-id assoc :layout new-layout))))

(defn- handle-empty-workspace-drop
  "Handle drop on an empty workspace — create a tile for the dropped entity."
  [e ws-id x y workspaces on-workspaces-change]
  (.preventDefault e)
  (let [raw (.getData (.-dataTransfer e) "text/plain")]
    (try
      (let [data (js/JSON.parse raw)
            entity-id (.-entityId data)]
        (when entity-id
          (let [actual-id (or ws-id (util/generate-id))
                workspaces (if ws-id workspaces
                               (assoc workspaces actual-id {:x x :y y :layout nil}))
                new-layout {:type :tile :id (util/generate-id) :entity-id entity-id}
                updated (update-workspaces-with-cleanup workspaces actual-id new-layout)]
            (on-workspaces-change updated))))
      (catch :default _ nil))))

(defn- grid-cell
  "Render a single grid cell (workspace or empty placeholder)."
  [ws-id workspace x y active? props]
  (let [{:keys [entities render-entity-tile active-entity
                on-workspaces-change on-entity-close
                on-active-entity-change on-entity-color-change workspaces]} props]
    [:div {:class (str "iris-grid-cell"
                       (when active? " iris-grid-cell-active"))
           :data-position (str x "," y)}
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
               (update-workspaces-with-cleanup workspaces ws-id new-layout))))
         :on-entity-close on-entity-close}]
       [:div.iris-empty-workspace
        {:on-drag-over (fn [e] (.preventDefault e))
         :on-drop (fn [e]
                    (when on-workspaces-change
                      (handle-empty-workspace-drop e ws-id x y workspaces on-workspaces-change)))}])]))

(defn- grid-canvas
  "Render the CSS grid canvas with all workspace cells."
  [cols rows workspaces active-position props]
  (let [[ax ay] active-position]
    (for [y (range rows)]
      (for [x (range cols)]
        (let [[ws-id workspace] (util/workspace-at workspaces x y)]
          ^{:key (str x "," y)}
          [grid-cell ws-id workspace x y (and (= x ax) (= y ay)) props])))))

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
          (let [{:keys [source-tile-id source-entity-id]} drop-info
                {:keys [workspaces active-position on-workspaces-change]} @props-ref
                [ax ay] active-position
                [active-id active-ws] (util/workspace-at workspaces ax ay)]
            (when (and source-entity-id on-workspaces-change active-id)
              (let [new-tile {:type :tile :id (util/generate-id) :entity-id source-entity-id}
                    existing-layout (:layout active-ws)
                    new-layout (if existing-layout
                                 {:type :split :id (util/generate-id)
                                  :direction "horizontal" :ratio 0.5
                                  :children [existing-layout new-tile]}
                                 new-tile)
                    updated (reduce-kv
                              (fn [acc ws-id ws-data]
                                (if (= ws-id active-id)
                                  (assoc acc ws-id (assoc ws-data :layout new-layout))
                                  (assoc acc ws-id (assoc ws-data :layout
                                                     (layout/remove-tile-from-layout (:layout ws-data) source-tile-id)))))
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
         [command-center/command-center-overlay cols rows workspaces active-position
          entities render-entity-tile on-active-position-change
          on-workspaces-change command-center?]]))))

;; ============================================================
;; JS <-> CLJS: Grid conversions
;; ============================================================

(defn js->workspaces
  "Convert a JS workspaces object {uuid: {x, y, layout}} to CLJS map.
   Workspace IDs stay as strings; values are fully keywordized."
  [js-obj]
  (when js-obj
    (into {}
          (map (fn [k] [k (js->clj (unchecked-get js-obj k) :keywordize-keys true)]))
          (js/Object.keys js-obj))))

(defn- iris-layout
  "Wrapper that converts JS props to CLJS for grid-component."
  [{:keys [workspaces activePosition activeEntity entities renderEntityTile
           onWorkspacesChange onActivePositionChange onActiveEntityChange onEntityClose
           onEntityColorChange logo]}]
  [grid-component
   {:workspaces (js->workspaces workspaces)
    :active-position (js->clj activePosition)
    :active-entity activeEntity
    :entities (js->clj entities :keywordize-keys true)
    :render-entity-tile renderEntityTile
    :on-workspaces-change (when onWorkspacesChange
                            (fn [new-workspaces]
                              (onWorkspacesChange (clj->js new-workspaces))))
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

     workspaces              - Object keyed by UUID strings.
                               Each value: { x, y, layout: LayoutNode }
                               LayoutNode is either:
                                 { type: 'tile',  id, 'entity-id' }
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
  (r/reactify-component iris-layout))
