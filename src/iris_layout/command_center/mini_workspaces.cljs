(ns iris-layout.command-center.mini-workspaces
  "Mini workspace grid for the command center — drag, drop, and preview."
  (:require [reagent.core :as r]
            [react-dom :as react-dom]
            ["@dnd-kit/core" :refer [DndContext DragOverlay PointerSensor useDraggable useDroppable useSensor useSensors]]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.interactions.grid :as grid-interactions]))

(def dnd-context (r/adapt-react-class DndContext))
(def drag-overlay (r/adapt-react-class DragOverlay))

;; True while a drag gesture is in progress — used by the overlay backdrop.
(def dragging? (r/atom false))

;; Drag ghost state: entity info + mouse position, rendered via portal to body.
(def drag-ghost (r/atom nil))

(defn on-mousemove [e]
  (when @drag-ghost
    (swap! drag-ghost assoc :x (.-clientX e) :y (.-clientY e))))

(defn on-drag-start-handler [entities]
  (fn [event]
    (let [^js data (.. event -active -data -current)
          entity-id (.-entity_id data)
          entity (get entities (keyword entity-id))
          ^js activate-evt (.-activatorEvent event)
          ^js active-el (.-target activate-evt)
          rect (when active-el (.getBoundingClientRect active-el))]
      (reset! dragging? true)
      (reset! drag-ghost {:name   (:name entity)
                          :color  (or (:color entity) "#6366f1")
                          :width  (if rect (.-width rect) 60)
                          :height (if rect (.-height rect) 30)
                          :offset-x (when rect (- (.-clientX activate-evt) (.-left rect)))
                          :offset-y (when rect (- (.-clientY activate-evt) (.-top rect)))
                          :x (.-clientX activate-evt)
                          :y (.-clientY activate-evt)})
      (.addEventListener js/window "mousemove" on-mousemove))))

(defn on-drag-end-handler [workspaces on-workspaces-change]
  (fn [event]
    (reset! dragging? false)
    (reset! drag-ghost nil)
    (.removeEventListener js/window "mousemove" on-mousemove)
    (let [active (.-active event)
          over   (.-over event)]
      (when (and active over)
        (let [tile-id    (.-id active)
              ^js data   (.. active -data -current)
              entity-id  (.-entity_id data)
              source-pos (vec (js->clj (.-source_pos data)))
              [tx ty]    (map js/parseInt (.split (.-id over) ","))
              target-pos [tx ty]]
          (when (not= source-pos target-pos)
            (let [source-ws     (get workspaces source-pos)
                  source-layout (layout/remove-tile-from-layout (:layout source-ws) tile-id)
                  target-ws     (or (get workspaces target-pos) {:layout nil})
                  new-tile      {:type :tile :id (util/generate-id) :entity-id entity-id}
                  target-layout (if (:layout target-ws)
                                  {:type :split :id (util/generate-id)
                                   :direction "horizontal" :ratio 0.5
                                   :children [(:layout target-ws) new-tile]}
                                  new-tile)
                  cleaned  (if source-layout
                             (assoc workspaces source-pos (assoc source-ws :layout source-layout))
                             (dissoc workspaces source-pos))
                  updated  (assoc cleaned target-pos (assoc target-ws :layout target-layout))]
              (on-workspaces-change updated))))))))

(defn mini-tile-fc [{:keys [layout-node entities on-close-entity source-pos on-navigate]}]
  (let [entity    (get entities (keyword (:entity-id layout-node)))
        result    (useDraggable #js {:id   (:id layout-node)
                                     :data #js {:entity_id  (:entity-id layout-node)
                                                :source_pos (clj->js source-pos)}})
        set-ref   (.-setNodeRef result)
        listeners (js->clj (.-listeners result) :keywordize-keys true)
        attrs     (js->clj (.-attributes result) :keywordize-keys true)
        dragging  (.-isDragging result)
        transform (.-transform result)
        style     (merge
                    (when (:color entity) {"--iris-tile-color" (:color entity)})
                    (when dragging {:opacity "0"}))]
    [:div.iris-command-center-tile
     (merge {:ref set-ref :style style
             :on-click (fn [e]
                         (.stopPropagation e)
                         (when (and on-navigate (not @dragging?))
                           (on-navigate)))}
            listeners attrs)
     [:span.iris-command-center-tile-name (:name entity)]
     [:button.iris-command-center-tile-close
      {:on-click        (fn [e] (.stopPropagation e) (on-close-entity (:id layout-node)))
       :on-pointer-down (fn [e] (.stopPropagation e))}
      "\u00d7"]]))

(defn command-center-mini-layout
  "Renders a miniature read-only preview of a layout tree inside the command center."
  [layout-node entities on-close-entity source-pos on-navigate]
  (case (layout/node-type layout-node)
    :tile
    [mini-tile-fc {:layout-node     layout-node
                   :entities        entities
                   :on-close-entity on-close-entity
                   :source-pos      source-pos
                   :on-navigate     on-navigate}]

    :split
    (let [[c1 c2] (:children layout-node)
          horiz?  (= (:direction layout-node) "horizontal")
          ratio   (:ratio layout-node 0.5)]
      [:div.iris-command-center-split
       {:class (if horiz? "horizontal" "vertical")}
       [:div {:style {:flex (str ratio)}}
        [command-center-mini-layout c1 entities on-close-entity source-pos on-navigate]]
       [:div {:style {:flex (str (- 1 ratio))}}
        [command-center-mini-layout c2 entities on-close-entity source-pos on-navigate]]])

    nil))

(defn cell-on-close-tile
  "Handle closing a tile from the mini layout preview."
  [pos workspace workspaces on-workspaces-change]
  (fn [tile-id]
    (when on-workspaces-change
      (let [new-layout (layout/remove-tile-from-layout (:layout workspace) tile-id)]
        (on-workspaces-change
          (if new-layout
            (assoc workspaces pos (assoc workspace :layout new-layout))
            (dissoc workspaces pos)))))))

(defn grid-cell-class
  "Build CSS class string for a workspace cell."
  [active? drop-target?]
  (str "iris-command-center-cell"
       (when active?      " iris-command-center-cell-active")
       (when drop-target? " iris-command-center-cell-drop-target")))

(defn grid-cell-fc [{:keys [pos workspace workspaces active-pos entities
                             on-workspaces-change on-active-position-change command-center?]}]
  (let [result      (useDroppable #js {:id (str (first pos) "," (second pos))})
        set-ref     (.-setNodeRef result)
        is-over     (.-isOver result)
        active?     (= pos active-pos)
        has-layout? (some? (:layout workspace))]
    [:div {:ref   set-ref
           :class (grid-cell-class active? is-over)
           :on-click (fn [e]
                       (.stopPropagation e)
                       (when-not @dragging?
                         (grid-interactions/navigate-to-workspace
                           pos on-active-position-change)))}
     (when has-layout?
       [command-center-mini-layout (:layout workspace) entities
        (cell-on-close-tile pos workspace workspaces on-workspaces-change)
        pos
        (fn [] (grid-interactions/navigate-to-workspace
                 pos on-active-position-change))])]))

(defn grid-view-size [cols rows]
  [(inc cols) (inc rows)])

(defn grid-style [view-cols view-rows]
  {:grid-template-columns (str "repeat(" view-cols ", 1fr)")
   :grid-template-rows    (str "repeat(" view-rows ", 1fr)")
   :aspect-ratio          (str view-cols " / " view-rows)})

(defn grid-cells [{:keys [view-cols view-rows workspaces active-position entities
                          on-workspaces-change on-active-position-change command-center?]}]
  [:<>
   (map (fn [[x y]]
          ^{:key (str x "," y)}
          [grid-cell-fc {:pos                       [x y]
                         :workspace                 (get workspaces [x y])
                         :workspaces                workspaces
                         :active-pos                active-position
                         :entities                  entities
                         :on-workspaces-change      on-workspaces-change
                         :on-active-position-change on-active-position-change
                         :command-center?           command-center?}])
        (for [y (range view-rows)
              x (range view-cols)]
          [x y]))])

(defn drag-ghost-portal []
  (when-let [g @drag-ghost]
    (react-dom/createPortal
      (r/as-element
        [:div.iris-command-center-tile
         {:style {:position "fixed"
                  :left     (- (:x g) (or (:offset-x g) 0))
                  :top      (- (:y g) (or (:offset-y g) 0))
                  :width    (:width g)
                  :height   (:height g)
                  :z-index  10000
                  :pointer-events "none"
                  "--iris-tile-color" (or (:color g) "#6366f1")}}
         [:span.iris-command-center-tile-name (:name g)]])
      js/document.body)))

(defn workspace-grid
  "Render the grid of all workspace cells."
  [{:keys [cols rows workspaces active-position entities
           on-workspaces-change on-active-position-change command-center?]}]
  (let [[view-cols view-rows] (grid-view-size cols rows)
        sensors (useSensors (useSensor PointerSensor #js {:activationConstraint #js {:distance 5}}))]
    [:<>
     [dnd-context {:sensors       sensors
                   :on-drag-start (on-drag-start-handler entities)
                   :on-drag-end   (on-drag-end-handler workspaces on-workspaces-change)}
      [:div.iris-command-center-grid
       {:style (grid-style view-cols view-rows)}
       [grid-cells {:view-cols                 view-cols
                    :view-rows                 view-rows
                    :workspaces                workspaces
                    :active-position           active-position
                    :entities                  entities
                    :on-workspaces-change      on-workspaces-change
                    :on-active-position-change on-active-position-change
                    :command-center?           command-center?}]]]
     [:f> drag-ghost-portal]]))
