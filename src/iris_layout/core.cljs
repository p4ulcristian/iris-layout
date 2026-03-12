(ns iris-layout.core
  "iris-layout — a tiling layout library for React.

   Exports two React components:
   - `Sidebar` : Renders stage cards with draggable entity cards
   - `Body`    : Renders the layout surface with resizable, rearrangeable entity tiles

   Both components accept the same props object:
   ```js
   {
     stages:              [{id, label, layout}],   // Array of stage definitions
     activeStage:         'stage-id',               // Currently active stage ID
     activeEntity:        'entity-id',              // Currently focused entity ID
     entities:            {id: {id, name, ...}},    // Entity data map
     renderEntityCard:    ReactComponent,           // Compact card for sidebar
     renderEntityTile:    ReactComponent,           // Full content for body tiles
     onStagesChange:      fn(stages),               // Layout mutations callback
     onActiveStageChange: fn(stageId),              // Stage switch callback
     onActiveEntityChange: fn(entityId),            // Entity focus callback
   }
   ```

   Layout tree structure (nested):
   - Tile:  {type: 'tile',  id: str, entityId: str}
   - Split: {type: 'split', id: str, direction: 'horizontal'|'vertical',
             ratio: 0.0-1.0, children: [node, node]}"
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.components.entity-tile-group :as entity-tile-group]
            [iris-layout.components.entity-card-group :as entity-card-group]))

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

(defn js->stages
  "Convert a JS stages array to CLJS vector of stage maps."
  [js-arr]
  (when js-arr
    (mapv (fn [s]
            (let [obj (js->clj s)]
              {:id    (get obj "id")
               :label (get obj "label")
               :layout (js->layout (get obj "layout"))}))
          js-arr)))

(defn stages->js
  "Convert CLJS stages vector to JS array."
  [stages]
  (clj->js
    (mapv (fn [s]
            {"id"     (:id s)
             "label"  (:label s)
             "layout" (layout->js (:layout s))})
          stages)))

;; ============================================================
;; Body — layout surface with stage stack
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
                             target-tile (layout/find-tile layout tile-id)
                             same-tile? (and target-tile
                                             (= (:entity-id target-tile) entity-id))
                             before? (or (= half :left) (= half :top))]
                         (when-not same-tile?
                           (let [base-layout (if (= source-type :tile)
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
                               (on-layout-change new-layout))))))
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

(defn- stage-index
  "Find the index of a stage by ID."
  [stages id]
  (first (keep-indexed (fn [i s] (when (= (:id s) id) i)) stages)))

(defn- restart-animation!
  "Force restart a CSS animation on an element.
   Uses offsetHeight read to force reflow before re-applying animation."
  [^js el anim-name]
  (when el
    (set! (.-animation (.-style el)) "none")
    (.-offsetHeight el)
    (set! (.-animation (.-style el)) (str anim-name " 400ms cubic-bezier(0.22, 1, 0.36, 1) forwards"))))

(defn body-component
  "Body — the main layout area.

   Renders all stages stacked on top of each other. The active stage
   is visible and interactive; others are hidden but stay mounted.

   Stage transitions use vertical slide animations:
   - Moving to a higher-indexed stage slides from bottom
   - Moving to a lower-indexed stage slides from top

   Props (received as CLJS map after wrapper conversion):
   :stages                  - vector of stage maps
   :active-stage            - active stage ID
   :active-entity           - focused entity ID
   :entities                - entity data map
   :render-entity-tile      - React component for tile content
   :on-stages-change        - fn(stages) for layout mutations
   :on-active-stage-change  - fn(stage-id) for stage switches
   :on-active-entity-change - fn(entity-id) for entity focus"
  [_]
  (let [prev-stage-id (atom nil)
        slide-dir (atom "right")
        layer-refs (atom {})]
    (fn [{:keys [stages active-stage active-entity entities render-entity-tile
                 on-stages-change on-active-stage-change on-active-entity-change on-entity-close]}]
      (let [prev-idx (stage-index stages @prev-stage-id)
            curr-idx (stage-index stages active-stage)
            changed? (and @prev-stage-id (not= @prev-stage-id active-stage))]
        ;; Determine slide direction based on stage order
        (when changed?
          (reset! slide-dir (if (and prev-idx curr-idx (> curr-idx prev-idx))
                              "down" "up")))
        ;; Trigger slide animation after Reagent renders the DOM
        (when changed?
          (let [dir @slide-dir
                target-id active-stage]
            (r/after-render
              (fn []
                (when-let [el (get @layer-refs target-id)]
                  (restart-animation! el (if (= dir "down")
                                           "iris-slide-from-bottom"
                                           "iris-slide-from-top")))))))
        (reset! prev-stage-id active-stage)
        [:div.iris-body
         ;; All stages stacked — each is always rendered
         [:div.iris-body-stack
          (for [stage stages]
            ^{:key (:id stage)}
            [:div {:class (str "iris-body-layer"
                               (when (= (:id stage) active-stage) " iris-body-layer-active"))
                   :ref (fn [el] (when el (swap! layer-refs assoc (:id stage) el)))}
             [body-stage-component
              {:layout (:layout stage)
               :entities entities
               :render-entity-tile render-entity-tile
               :active-entity active-entity
               :on-layout-change
               (fn [new-layout]
                 (when on-stages-change
                   (let [updated (mapv (fn [s]
                                         (if (= (:id s) (:id stage))
                                           (assoc s :layout new-layout)
                                           s))
                                       stages)]
                     (on-stages-change updated))))
               :on-entity-close on-entity-close}]])]]))))

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
  "Return hiccup for a navigation edge button."
  [css-class arrow visible? on-click]
  [:div {:class (str "iris-nav-edge " css-class
                     (when-not visible? " iris-nav-hidden"))
         :on-click on-click}
   [:span.iris-nav-arrow arrow]])

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
             (on-workspaces-change (assoc workspaces k {:layout new-layout}))))
         :on-entity-close on-entity-close}]
       [:div.iris-empty-workspace
        [:div.iris-empty-workspace-label k]])]))

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

(def ^:private grid-gap 32)

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
                                (handle-nav dir)))))
        keyup-handler (fn [e]
                        (when (= (.-key e) "Alt")
                          (reset! zoomed-out? false)))]
    (.addEventListener js/document "keydown" keydown-handler)
    (.addEventListener js/document "keyup" keyup-handler)
    (fn [{:keys [workspaces active-position] :as props}]
      (reset! props-ref props)
      (let [[cols rows] (grid-dimensions workspaces)
            zoomed? @zoomed-out?
            vis? (fn [dir] (can-navigate? dir workspaces active-position))]
        [:div.iris-grid-viewport
         (nav-edge-hiccup "iris-nav-left" "\u2039" (vis? :left) #(handle-nav :left))
         (nav-edge-hiccup "iris-nav-right" "\u203A" (vis? :right) #(handle-nav :right))
         (nav-edge-hiccup "iris-nav-top" "\u2039" (vis? :up) #(handle-nav :up))
         (nav-edge-hiccup "iris-nav-bottom" "\u203A" (vis? :down) #(handle-nav :down))
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
;; Sidebar — stage cards with entity cards
;; ============================================================

(defn- remove-entity-from-stages
  "Remove an entity from a specific stage. If the stage becomes empty, remove it.
   Returns [updated-stages removed-stage-empty?]."
  [stages source-stage-id entity-id]
  (let [source-stage (first (filter #(= (:id %) source-stage-id) stages))
        new-layout (when source-stage
                     (layout/remove-entity-from-layout (:layout source-stage) entity-id))
        empty? (nil? new-layout)]
    [(if empty?
       (vec (remove #(= (:id %) source-stage-id) stages))
       (mapv (fn [s]
               (if (= (:id s) source-stage-id)
                 (assoc s :layout new-layout)
                 s))
             stages))
     empty?]))

(defn sidebar-component
  "Sidebar — renders a list of stage cards with drag-drop support.

   Features:
   - Click a card to switch active stage
   - Click an entity card to focus it
   - Close button removes entity from stage (empty stage is removed)
   - Drag entity cards between groups to move them
   - Drag entity cards to empty sidebar area to create a new group

   Props (received as CLJS map after wrapper conversion):
   :stages                  - vector of stage maps
   :active-stage            - active stage ID
   :active-entity           - focused entity ID
   :entities                - entity data map
   :render-entity-card      - React component for entity card content
   :on-stages-change        - fn(stages) for layout mutations
   :on-active-stage-change  - fn(stage-id) for stage switches
   :on-active-entity-change - fn(entity-id) for entity focus
   :on-entity-close         - (optional) fn(stage-id, entity-id) side-effect hook called on close"
  [_]
  (let [drag-over (r/atom false)]
    (fn [{:keys [stages active-stage active-entity entities render-entity-card
                 on-stages-change on-active-stage-change on-active-entity-change
                 on-entity-close]}]
      [:div.iris-sidebar
       {:class (when @drag-over "iris-sidebar-drag-over")
        :on-drag-over (fn [e] (.preventDefault e) (reset! drag-over true))
        :on-drag-enter (fn [e] (.preventDefault e))
        :on-drag-leave (fn [e]
                         (when (not (.contains (.-currentTarget e) (.-relatedTarget e)))
                           (reset! drag-over false)))
        :on-drop
        (fn [e]
          (.preventDefault e)
          (reset! drag-over false)
          ;; Only handle if not caught by a group (stopPropagation)
          (let [raw (.getData (.-dataTransfer e) "text/plain")]
            (try
              (let [data (js/JSON.parse raw)]
                (when (and (= (.-source data) "sidebar")
                           (.-entityId data)
                           (.-stageId data)
                           on-stages-change)
                  (let [entity-id (.-entityId data)
                        source-stage-id (.-stageId data)
                        [updated _] (remove-entity-from-stages stages source-stage-id entity-id)
                        new-stage-id (generate-id)
                        new-stage {:id new-stage-id
                                   :label (or (:name (get entities entity-id)) "New Stage")
                                   :layout {:type :tile
                                            :id (generate-id)
                                            :entity-id entity-id}}]
                    (on-stages-change (conj updated new-stage))
                    (when on-active-stage-change
                      (on-active-stage-change new-stage-id)))))
              (catch :default _ nil))))}
       (for [stage stages]
         ^{:key (:id stage)}
         [entity-card-group/entity-card-group-component
          {:stage stage
           :active? (= (:id stage) active-stage)
           :entities entities
           :active-entity active-entity
           :render-entity-card render-entity-card
           :on-click #(when on-active-stage-change
                        (on-active-stage-change (:id stage)))
           :on-entity-click #(when on-active-entity-change
                               (on-active-entity-change %))
           :on-entity-close
           (fn [entity-id]
             (when on-stages-change
               (let [[updated empty?] (remove-entity-from-stages stages (:id stage) entity-id)]
                 (on-stages-change updated)
                 (when (and empty?
                            (= active-stage (:id stage))
                            on-active-stage-change
                            (seq updated))
                   (on-active-stage-change (:id (first updated))))))
             (when on-entity-close
               (on-entity-close (:id stage) entity-id)))
           :on-entity-drop
           (fn [entity-id source-stage-id]
             ;; Move entity from source stage to this stage
             (when on-stages-change
               (let [[updated source-empty?] (remove-entity-from-stages stages source-stage-id entity-id)
                     ;; Add entity to target stage's layout
                     updated (mapv (fn [s]
                                     (if (= (:id s) (:id stage))
                                       (assoc s :layout
                                              (layout/append-entity
                                                (:layout s) entity-id
                                                (generate-id) (generate-id)))
                                       s))
                                   updated)]
                 (on-stages-change updated)
                 ;; If source was active and got removed, switch to target
                 (when (and source-empty?
                            (= active-stage source-stage-id)
                            on-active-stage-change)
                   (on-active-stage-change (:id stage))))))}])])))

;; ============================================================
;; React-facing wrappers (JS consumers)
;; ============================================================
;; reactify-component auto-converts JS props to CLJS map with keyword keys.
;; Nested objects (layout, entities, stages) remain as JS and need manual conversion.

(defn- body-wrapper
  "Wrapper that converts JS props to CLJS for body-component."
  [{:keys [stages activeStage activeEntity entities renderEntityTile
           onStagesChange onActiveStageChange onActiveEntityChange onEntityClose]}]
  [body-component
   {:stages (js->stages stages)
    :active-stage activeStage
    :active-entity activeEntity
    :entities (js->entities entities)
    :render-entity-tile renderEntityTile
    :on-stages-change (when onStagesChange
                        (fn [new-stages]
                          (onStagesChange (stages->js new-stages))))
    :on-active-stage-change onActiveStageChange
    :on-active-entity-change onActiveEntityChange
    :on-entity-close onEntityClose}])

(defn- sidebar-wrapper
  "Wrapper that converts JS props to CLJS for sidebar-component."
  [{:keys [stages activeStage activeEntity entities renderEntityCard
           onStagesChange onActiveStageChange onActiveEntityChange onEntityClose]}]
  [sidebar-component
   {:stages (js->stages stages)
    :active-stage activeStage
    :active-entity activeEntity
    :entities (js->entities entities)
    :render-entity-card renderEntityCard
    :on-stages-change (when onStagesChange
                        (fn [new-stages]
                          (onStagesChange (stages->js new-stages))))
    :on-active-stage-change onActiveStageChange
    :on-active-entity-change onActiveEntityChange
    :on-entity-close onEntityClose}])

;; ============================================================
;; Exported React components
;; ============================================================

(def Sidebar
  "React component — stage list with draggable entity cards.
   See namespace docstring for full props specification."
  (r/reactify-component sidebar-wrapper))

(def Body
  "React component — layout surface with resizable, rearrangeable entity tiles.
   See namespace docstring for full props specification."
  (r/reactify-component body-wrapper))

(def Grid
  "React component — 2D workspace grid with Alt+Arrow navigation.
   Workspaces are addressed by [x,y] coordinates in an infinite grid."
  (r/reactify-component grid-wrapper))
