(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for Reagent."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            ["@dnd-kit/core" :refer [DndContext useDroppable]]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.pane.pane :as pane]
            [iris-layout.pane.tile :as tile]
            [iris-layout.command-center.core :as command-center]
            [iris-layout.interactions.tile :as tile-interactions]
            [iris-layout.interactions.grid :as grid-interactions]))

(r/set-default-compiler! (r/create-compiler {:function-components true}))

(def dnd-context-component (r/adapt-react-class DndContext))


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

(defn nav-edge-fc
  "Droppable navigation edge — hovering a dragged tile triggers workspace navigation."
  [{:keys [css-class visible? on-click direction]}]
  (let [result  (useDroppable #js {:id (str "nav:" (name direction))})
        set-ref (.-setNodeRef result)
        is-over (.-isOver result)]
    [:div {:ref   set-ref
           :class (str "iris-nav-edge " css-class
                       (when-not visible? " iris-nav-hidden")
                       (when is-over " iris-nav-drag-over"))
           :on-click on-click}
     [:div.iris-nav-semicircle]]))

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

(defn handle-drag-start [event]
  (reset! tile/dragging-tile (.. event -active -id)))

(defn update-tile-drop-target!
  "Update the global drop-target atom based on the current drag position over a tile."
  [active over over-id]
  (if (and over-id (str/starts-with? over-id "tile:"))
    (let [target-tile-id (subs over-id 5)
          active-rect    (.. active -rect -current -translated)
          over-rect      (.-rect over)]
      (when (and active-rect over-rect)
        (reset! tile/drop-target
          {:tile-id target-tile-id
           :half    (tile/calculate-half-from-rects active-rect over-rect)})))
    (reset! tile/drop-target nil)))

(defn handle-nav-edge-hover
  "Navigate to an adjacent workspace when the drag first enters a nav edge."
  [over-id prev-nav-over props-ref]
  (when (and over-id
             (str/starts-with? over-id "nav:")
             (not= over-id @prev-nav-over))
    (grid-interactions/handle-grid-nav (keyword (subs over-id 4)) props-ref)))

(defn handle-drag-move [props-ref prev-nav-over event]
  (let [active  (.-active event)
        over    (.-over event)
        over-id (when over (.-id over))]
    (update-tile-drop-target! active over over-id)
    (handle-nav-edge-hover over-id prev-nav-over props-ref)
    (reset! prev-nav-over over-id)))

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

(defn handle-drag-end [props-ref prev-nav-over event]
  (let [active    (.-active event)
        over      (.-over event)
        over-id   (when over (.-id over))
        dt        @tile/drop-target
        tile-id   (.. active -data -current -tile-id)
        entity-id (.. active -data -current -entity-id)
        {:keys [workspaces on-workspaces-change]} @props-ref]
    (reset! tile/drop-target nil)
    (reset! tile/dragging-tile nil)
    (reset! prev-nav-over nil)
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
        props-ref       (atom nil)
        prev-nav-over   (atom nil)]
    (.addEventListener js/document "keydown" (partial handle-keydown command-center? props-ref))
    (.addEventListener js/document "keyup"   (partial handle-keyup command-center?))
    (fn [{:keys [workspaces active-position entities render-entity-tile
                 on-active-position-change logo] :as props}]
      (reset! props-ref props)
      (let [[cols rows] (grid-dimensions workspaces)
            dragging?   (boolean @tile/dragging-tile)
            vis?        (fn [dir] (or dragging? (grid-interactions/can-navigate? dir workspaces active-position)))
            nav-handler (fn [dir] #(grid-interactions/handle-grid-nav dir props-ref))]
        [:div.iris-grid-viewport
         {:style (when @tile/dragging-tile {:cursor "grabbing"})}
         [dnd-context-component
          {:on-drag-start handle-drag-start
           :on-drag-move  (partial handle-drag-move props-ref prev-nav-over)
           :on-drag-end   (partial handle-drag-end props-ref prev-nav-over)}
          [nav-edge-fc {:css-class "iris-nav-left"   :visible? (vis? :left)  :on-click (nav-handler :left)  :direction :left}]
          [nav-edge-fc {:css-class "iris-nav-right"  :visible? (vis? :right) :on-click (nav-handler :right) :direction :right}]
          [nav-edge-fc {:css-class "iris-nav-top"    :visible? (vis? :up)    :on-click (nav-handler :up)    :direction :up}]
          [nav-edge-fc {:css-class "iris-nav-bottom" :visible? (vis? :down)  :on-click (nav-handler :down)  :direction :down}]
          [:div.iris-grid-center
           [:div.iris-grid-canvas
            {:style (camera-style cols rows active-position)}
            (grid-canvas cols rows workspaces active-position props)]]
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
