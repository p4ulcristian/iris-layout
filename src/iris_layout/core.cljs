(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for Reagent."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            ["@dnd-kit/core" :refer [DndContext DragOverlay useDroppable]]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.pane.pane :as pane]
            [iris-layout.pane.tile :as tile]
            [iris-layout.command-center.core :as command-center]
            [iris-layout.interactions.tile :as tile-interactions]
            [iris-layout.interactions.grid :as grid-interactions]
            [iris-layout.schema :as schema]))

(r/set-default-compiler! (r/create-compiler {:function-components true}))

(def dnd-context-component (r/adapt-react-class DndContext))
(def drag-overlay-component (r/adapt-react-class DragOverlay))


;; ============================================================
;; Body stage — single workspace layout renderer
;; ============================================================

(defn body-stage-component
  [{:keys [layout entities render-entity-tile active-entity
           on-layout-change on-active-entity-change on-entity-color-change on-entity-close]}]
  [:div.iris-body-stage
   [pane/pane
    layout
    (partial tile-interactions/handle-close layout on-layout-change on-entity-close)
    (partial tile-interactions/handle-ratio layout on-layout-change)
    active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]])


;; ============================================================
;; Grid components
;; ============================================================

(defn grid-dimensions
  "Compute grid bounds from workspace [x y] keys. Returns [cols rows]."
  [workspaces]
  (if (empty? workspaces)
    [1 1]
    [(inc (apply max (map first (keys workspaces))))
     (inc (apply max (map second (keys workspaces))))]))

(defn empty-workspace-fc
  "Droppable empty workspace cell."
  [{:keys [x y]}]
  (let [result  (useDroppable #js {:id (str "empty:" x "," y)})
        set-ref (.-setNodeRef result)
        is-over (.-isOver result)]
    [:div.iris-empty-workspace
     {:ref   set-ref
      :style (when is-over {:background "rgba(59,130,246,0.1)"})}]))


(defn grid-cell
  [workspace x y active? props]
  (let [{:keys [entities render-entity-tile active-entity
                on-workspaces-change on-entity-close
                on-active-entity-change on-entity-color-change workspaces]} props
        pos [x y]]
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
               (assoc workspaces pos (assoc workspace :layout new-layout)))))
         :on-entity-close on-entity-close}]
       [empty-workspace-fc {:x x :y y}])]))

(defn grid-canvas
  [cols rows workspaces active-position props]
  (let [[ax ay] active-position]
    (for [y (range rows)
          x (range cols)]
      ^{:key (str x "," y)}
      [grid-cell (get workspaces [x y]) x y (and (= x ax) (= y ay)) props])))

(def ^:private grid-gap 16)

(defn camera-style
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


;; ============================================================
;; Drag handlers
;; ============================================================

(defn handle-drag-start [props-ref event]
  (let [active       (.-active event)
        ^js data     (.. active -data -current)
        entity-id    (.-entity_id data)
        entities     (:entities @props-ref)
        entity       (get entities (keyword entity-id))
        color        (or (:color entity) "#6366f1")
        ^js act-evt  (.-activatorEvent event)
        ^js active-el (.-target act-evt)
        tile-el      (when active-el (.closest active-el ".iris-entity-tile"))
        rect         (when tile-el (.getBoundingClientRect tile-el))]
    (reset! tile/dragging-tile (.-id active))
    (reset! tile/dragging-entity {:entity-id entity-id
                                  :name      (:name entity)
                                  :color     color})
    (reset! tile/drag-ghost {:name     (:name entity)
                             :color    color
                             :width    (if rect (.-width rect) 200)
                             :height   (if rect (.-height rect) 32)
                             :offset-x (when rect (- (.-clientX act-evt) (.-left rect)))
                             :offset-y (when rect (- (.-clientY act-evt) (.-top rect)))
                             :x        (.-clientX act-evt)
                             :y        (.-clientY act-evt)})
    (.addEventListener js/window "mousemove" tile/on-mousemove)))

(defn update-tile-drop-target!
  "Update the global drop-target atom based on the pointer position over a tile."
  [over over-id]
  (if (and over-id (str/starts-with? over-id "tile:"))
    (let [target-tile-id (subs over-id 5)
          ghost          @tile/drag-ghost
          over-rect      (.-rect over)]
      (when (and ghost over-rect)
        (reset! tile/drop-target
          {:tile-id target-tile-id
           :half    (tile/calculate-half-from-pointer (:x ghost) (:y ghost) over-rect)})))
    (reset! tile/drop-target nil)))

(defn handle-drag-move [props-ref event]
  (let [over    (.-over event)
        over-id (when over (.-id over))]
    (update-tile-drop-target! over over-id)))

(defn handle-tile-split
  "Execute a tile split when a drag is dropped onto another tile."
  [workspaces on-workspaces-change tile-id entity-id dt]
  (let [target-tile-id (:tile-id dt)
        half           (:half dt)
        direction      (tile/half->direction half)
        before?        (or (= half :left) (= half :top))]
    (when-let [[pos ws] (first (filter (fn [[_ ws]]
                                         (layout/find-tile (:layout ws) target-tile-id))
                                       workspaces))]
      (let [base-layout (or (layout/remove-tile-from-layout (:layout ws) tile-id) (:layout ws))
            new-layout  (layout/split-tile base-layout target-tile-id direction
                                           entity-id (util/generate-id) (util/generate-id) before?)]
        (when new-layout
          (on-workspaces-change
            (tile-interactions/update-workspaces-with-cleanup workspaces pos new-layout tile-id)))))))

(defn handle-drag-end [props-ref event]
  (let [active    (.-active event)
        over      (.-over event)
        over-id   (when over (.-id over))
        dt        @tile/drop-target
        ^js data  (.. active -data -current)
        tile-id   (.-tile_id data)
        entity-id (.-entity_id data)
        {:keys [workspaces on-workspaces-change]} @props-ref]
    (reset! tile/drop-target nil)
    (reset! tile/dragging-tile nil)
    (reset! tile/dragging-entity nil)
    (reset! tile/drag-ghost nil)
    (.removeEventListener js/window "mousemove" tile/on-mousemove)
    (when (and over-id tile-id on-workspaces-change)
      (cond
        (and (str/starts-with? over-id "tile:") dt)
        (handle-tile-split workspaces on-workspaces-change tile-id entity-id dt)

        (str/starts-with? over-id "empty:")
        (let [[x y] (map js/parseInt (.split (subs over-id 6) ","))]
          (grid-interactions/handle-empty-workspace-drop
            x y workspaces on-workspaces-change tile-id entity-id))))))

(defn handle-keydown [command-center? props-ref e]
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
        (grid-interactions/handle-grid-nav dir props-ref)))))

(defn handle-keyup [command-center? e]
  (when (= (.-key e) "Alt")
    (reset! command-center? false)))


;; ============================================================
;; Grid component
;; ============================================================

(defn command-center-trigger [command-center? logo]
  [:div.iris-command-center-trigger
   {:on-click (fn [_] (reset! command-center? true))}
   [:svg.iris-command-center-trigger-bg {:viewBox "0 0 48 48" :width "100%" :height "100%"}
    [:path {:d "M0 48 L0 0 A48 48 0 0 1 48 48 Z" :fill "currentColor"}]]
   [:div.iris-command-center-trigger-logo
    (or logo
        [:svg {:viewBox "0 0 24 24" :width "20" :height "20"}
         [:circle {:cx 12 :cy 12 :r 7 :fill "none" :stroke "currentColor" :stroke-width 1.5}]
         [:circle {:cx 12 :cy 12 :r 2.5 :fill "currentColor"}]])]])

(defn grid-component
  "Grid — a 2D grid of workspaces with camera-based navigation.

   Workspaces are a map keyed by [x y] position vectors:
     {[0 0] {:layout LayoutNode}
      [1 0] {:layout LayoutNode}}

   LayoutNode is either:
     {:type \"tile\"  :id string :entity-id string}
     {:type \"split\" :id string :direction \"horizontal\"|\"vertical\"
      :ratio 0-1 :children [LayoutNode LayoutNode]}

   Props (CLJS keywords):
     :workspaces               - {[x y] {:layout LayoutNode}}
     :active-position          - [x y]
     :active-entity            - entity-id string or nil
     :entities                 - map of entity-id → entity map
     :render-entity-tile       - Reagent component fn, receives entity map
     :on-workspaces-change     - fn(workspaces)
     :on-active-position-change - fn([x y])
     :on-active-entity-change  - fn(entity-id)
     :on-entity-close          - fn(entity-id)
     :on-entity-color-change   - fn(entity-id color)
     :logo                     - hiccup for the command center trigger icon"
  [_]
  (util/inject-css!)
  (let [command-center? (r/atom false)
        props-ref       (atom nil)]
    (.addEventListener js/document "keydown" (partial handle-keydown command-center? props-ref))
    (.addEventListener js/document "keyup"   (partial handle-keyup command-center?))
    (fn [{:keys [workspaces active-position entities render-entity-tile
                 on-active-position-change logo] :as props}]
      (schema/validate-grid-props props)
      (reset! props-ref props)
      (let [[cols rows] (grid-dimensions workspaces)]
        [:div.iris-grid-viewport
         {:style (when @tile/dragging-tile {:cursor "grabbing"})}
         [dnd-context-component
          {:on-drag-start (partial handle-drag-start props-ref)
           :on-drag-move  (partial handle-drag-move props-ref)
           :on-drag-end   (partial handle-drag-end props-ref)}
          [:div.iris-grid-center
           [:div.iris-grid-canvas
            {:style (camera-style cols rows active-position)}
            (grid-canvas cols rows workspaces active-position props)]]
          [drag-overlay-component]
          [:f> tile/drag-ghost-portal]
          [command-center-trigger command-center? logo]
          [command-center/command-center-overlay
           {:cols                    cols
            :rows                    rows
            :workspaces              workspaces
            :active-position         active-position
            :entities                entities
            :on-active-position-change on-active-position-change
            :on-workspaces-change    (:on-workspaces-change props)
            :command-center?         command-center?}]]]))))
