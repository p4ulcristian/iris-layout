(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for Reagent."
  (:require [cljs.pprint]
            [reagent.core :as r]
            [clojure.string :as str]
            ["@dnd-kit/core" :refer [DndContext DragOverlay PointerSensor useDroppable useSensor useSensors]]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.pane.pane :as pane]
            [iris-layout.pane.tile :as tile]
            [iris-layout.command-center.core :as command-center]
            [iris-layout.interactions.tile :as tile-interactions]
            [iris-layout.interactions.grid :as grid-interactions]
            [iris-layout.schema :as schema]))

(r/set-default-compiler! (r/create-compiler {:function-components true}))

(def drag-overlay-component (r/adapt-react-class DragOverlay))

(defonce debug-open? (r/atom false))

(defn debug-inspector
  "Floating state inspector — toggled with Alt+T. Shows props as EDN with copy button."
  [props]
  (when @debug-open?
    (let [copied? (r/atom false)
          edn-str (with-out-str (cljs.pprint/pprint
                    (select-keys props [:workspaces :active-position :active-entity :entities])))]
      [:div {:style {:position "fixed"
                     :bottom 4 :right 4
                     :width 280 :max-height 200
                     :z-index 99999
                     :background "rgb(15 20 35 / 0.92)"
                     :border "1px solid rgb(255 255 255 / 0.1)"
                     :border-radius 6
                     :overflow "hidden"
                     :display "flex"
                     :flex-direction "column"
                     :font-family "monospace"
                     :font-size 9
                     :color "rgb(255 255 255 / 0.7)"
                     :backdrop-filter "blur(16px)"}}
       [:div {:style {:display "flex" :align-items "center" :justify-content "space-between"
                      :padding "3px 6px"
                      :border-bottom "1px solid rgb(255 255 255 / 0.06)"
                      :flex-shrink 0}}
        [:span {:style {:font-weight 600 :font-size 9}} "state"]
        [:div {:style {:display "flex" :gap 4}}
         [:button {:style {:background (if @copied? "rgb(34 197 94 / 0.3)" "rgb(255 255 255 / 0.08)")
                           :border "none" :color "rgb(255 255 255 / 0.6)"
                           :padding "1px 6px" :border-radius 3 :cursor "pointer" :font-size 9}
                   :on-click (fn []
                               (.writeText js/navigator.clipboard edn-str)
                               (reset! copied? true)
                               (js/setTimeout #(reset! copied? false) 1500))}
          (if @copied? "ok" "cp")]
         [:button {:style {:background "rgb(255 255 255 / 0.08)"
                           :border "none" :color "rgb(255 255 255 / 0.6)"
                           :padding "1px 5px" :border-radius 3 :cursor "pointer" :font-size 9}
                   :on-click #(reset! debug-open? false)}
          "×"]]]
       [:pre {:style {:margin 0 :padding "4px 6px" :overflow "auto" :flex 1
                      :white-space "pre-wrap" :word-break "break-all" :line-height 1.3}}
        edn-str]])))

(defn dnd-context-fc
  "DndContext wrapper that configures a PointerSensor with a distance constraint,
   so that clicks and double-clicks still fire on draggable elements."
  [{:keys [on-drag-start on-drag-move on-drag-end children]}]
  (let [sensors (useSensors (useSensor PointerSensor #js {:activationConstraint #js {:distance 5}}))]
    (r/as-element
      [:> DndContext {:sensors      sensors
                      :onDragStart  on-drag-start
                      :onDragMove   on-drag-move
                      :onDragEnd    on-drag-end}
       children])))


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

(def grid-cols 3)
(def grid-rows 3)

(defn grid-dimensions
  "Fixed 3×3 grid. Returns [cols rows]."
  [_workspaces]
  [grid-cols grid-rows])

(defn empty-workspace-fc
  "Droppable empty workspace cell."
  [{:keys [x y cell-number]}]
  (let [result  (useDroppable #js {:id (str "empty:" x "," y)})
        set-ref (.-setNodeRef result)
        is-over (.-isOver result)]
    [:div.iris-empty-workspace
     {:ref   set-ref
      :style (when is-over {:background "rgba(59,130,246,0.1)"})}
     (when cell-number
       [:span.iris-workspace-number (str cell-number)])]))


(defn grid-cell
  [workspace x y active? props cell-number]
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
       [empty-workspace-fc {:x x :y y :cell-number cell-number}])]))

(defn grid-canvas
  [cols rows workspaces active-position props]
  (let [[ax ay] active-position]
    (map-indexed
      (fn [idx [x y]]
        ^{:key (str x "," y)}
        [grid-cell (get workspaces [x y]) x y (and (= x ax) (= y ay)) props (inc idx)])
      (for [y (range rows)
            x (range cols)]
        [x y]))))

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
                             :icon     (:icon entity)
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

(defn find-workspace-with-tile
  "Find the [pos workspace] entry containing a given tile-id."
  [workspaces tile-id]
  (first (filter (fn [[_ ws]] (layout/find-tile (:layout ws) tile-id))
                 workspaces)))

(defn split-and-cleanup
  "Split a target tile in a workspace and clean up the source tile."
  [workspaces on-workspaces-change pos ws tile-id entity-id direction before? target-tile-id]
  (when-let [new-layout (layout/split-tile (:layout ws) target-tile-id direction
                                           entity-id (util/generate-id) (util/generate-id) before?)]
    (on-workspaces-change
      (tile-interactions/update-workspaces-with-cleanup workspaces pos new-layout tile-id))))

(defn handle-tile-split
  "Execute a tile split when a drag is dropped onto another tile."
  [workspaces on-workspaces-change tile-id entity-id dt]
  (let [target-tile-id (:tile-id dt)
        half           (:half dt)
        direction      (tile/half->direction half)
        before?        (or (= half :left) (= half :top))]
    (when-not (= tile-id target-tile-id)
      (when-let [[pos ws] (find-workspace-with-tile workspaces target-tile-id)]
        (split-and-cleanup workspaces on-workspaces-change pos ws
                           tile-id entity-id direction before? target-tile-id)))))

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
  (when (= (.-key e) "Escape")
    (reset! command-center? false))
  (when (and (= (.-key e) "Alt") (not (.-repeat e)))
    (reset! command-center? true))
  (when (.-altKey e)
    (case (.-code e)
      "KeyT" (do (.preventDefault e)
                  (swap! debug-open? not))
      "KeyW" (when-let [props @props-ref]
               (.preventDefault e)
               (reset! command-center? false)
               (let [{:keys [workspaces active-position active-entity
                             on-workspaces-change on-entity-close
                             on-active-entity-change]} props
                     ws     (get workspaces active-position)
                     layout (:layout ws)]
                 (when-let [tile (when active-entity
                                   (layout/find-tile-by-entity layout active-entity))]
                   (let [new-layout (layout/remove-tile-from-layout layout (:id tile))
                         next-tile  (layout/last-tile new-layout)]
                     (when on-workspaces-change
                       (on-workspaces-change
                         (assoc workspaces active-position (assoc ws :layout new-layout))))
                     (when on-entity-close
                       (on-entity-close (:entity-id tile)))
                     (when on-active-entity-change
                       (on-active-entity-change
                         (when next-tile (:entity-id next-tile))))))))
      nil)
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
  ;; Reset any browser-restored scroll position that could offset the viewport
  (js/window.scrollTo 0 0)
  (when-let [app (.getElementById js/document "app")]
    (set! (.-scrollTop app) 0)
    (set! (.-scrollLeft app) 0))
  (let [command-center? (r/atom false)
        props-ref       (atom nil)
        mounted?        (r/atom false)]
    (.addEventListener js/document "keydown" (partial handle-keydown command-center? props-ref))
    (.addEventListener js/document "keyup"   (partial handle-keyup command-center?))
    (fn [{:keys [workspaces active-position entities render-entity-tile
                 on-active-position-change logo] :as props}]
      (schema/validate-grid-props props)
      (reset! props-ref props)
      (when-not @mounted?
        (js/requestAnimationFrame
          #(js/requestAnimationFrame
             (fn [] (reset! mounted? true)))))
      (let [[cols rows] (grid-dimensions workspaces)]
        [:div.iris-grid-viewport
         {:style (when @tile/dragging-tile {:cursor "grabbing"})}
         [:f> dnd-context-fc
          {:on-drag-start (partial handle-drag-start props-ref)
           :on-drag-move  (partial handle-drag-move props-ref)
           :on-drag-end   (partial handle-drag-end props-ref)
           :children
           (r/as-element
             [:<>
              [:div.iris-grid-center
               [:div {:class (str "iris-grid-canvas" (when-not @mounted? " iris-no-transition"))
                      :style (camera-style cols rows active-position)}
                (grid-canvas cols rows workspaces active-position props)]]
              [drag-overlay-component]
              [:f> tile/drag-ghost-portal]
              [command-center/command-center
               {:cols                    cols
                :rows                    rows
                :workspaces              workspaces
                :active-position         active-position
                :active-entity           (:active-entity props)
                :entities                entities
                :entity-types            (:entity-types props)
                :on-active-position-change on-active-position-change
                :on-workspaces-change    (:on-workspaces-change props)
                :on-entity-create        (:on-entity-create props)
                :on-active-entity-change (:on-active-entity-change props)
                :command-center?         command-center?
                :logo                    logo}]])}]
         [debug-inspector props]]))))
