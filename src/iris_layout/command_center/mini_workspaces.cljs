(ns iris-layout.command-center.mini-workspaces
  "Mini workspace grid for the command center — drag, drop, and preview."
  (:require [reagent.core :as r]
            ["@dnd-kit/core" :refer [DndContext useDraggable useDroppable]]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.interactions.grid :as grid-interactions]))

(def dnd-context (r/adapt-react-class DndContext))

;; True while a drag gesture is in progress — used by the overlay backdrop.
(def dragging? (r/atom false))

(defn on-drag-start-handler []
  (fn [_] (reset! dragging? true)))

(defn on-drag-end-handler [workspaces on-workspaces-change]
  (fn [event]
    (reset! dragging? false)
    (let [active (.-active event)
          over   (.-over event)]
      (when (and active over)
        (let [tile-id    (.-id active)
              data       (.. active -data -current)
              entity-id  (.-entity-id data)
              source-pos (vec (js->clj (.-source-pos data)))
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

(defn mini-tile-fc [{:keys [layout-node entities on-close-entity source-pos]}]
  (let [entity    (get entities (keyword (:entity-id layout-node)))
        result    (useDraggable #js {:id   (:id layout-node)
                                     :data (clj->js {:entity-id  (:entity-id layout-node)
                                                     :source-pos source-pos})})
        set-ref   (.-setNodeRef result)
        listeners (js->clj (.-listeners result) :keywordize-keys true)
        attrs     (js->clj (.-attributes result) :keywordize-keys true)
        dragging  (.-isDragging result)
        transform (.-transform result)
        style     (merge
                    (when (:color entity) {"--iris-tile-color" (:color entity)})
                    (when transform {:transform (str "translate3d(" (.-x transform) "px,"
                                                                   (.-y transform) "px,0)")})
                    (when dragging {:outline        "2px solid #fbbf24"
                                    :outline-offset "-2px"
                                    :opacity        "0.5"}))]
    [:div.iris-command-center-tile
     (merge {:ref set-ref :style style} listeners attrs)
     [:span.iris-command-center-tile-name (:name entity)]
     [:button.iris-command-center-tile-close
      {:on-click        (fn [e] (.stopPropagation e) (on-close-entity (:id layout-node)))
       :on-pointer-down (fn [e] (.stopPropagation e))}
      "\u00d7"]]))

(defn command-center-mini-layout
  "Renders a miniature read-only preview of a layout tree inside the command center."
  [layout-node entities on-close-entity source-pos]
  (case (layout/node-type layout-node)
    :tile
    [mini-tile-fc {:layout-node     layout-node
                   :entities        entities
                   :on-close-entity on-close-entity
                   :source-pos      source-pos}]

    :split
    (let [[c1 c2] (:children layout-node)
          horiz?  (= (:direction layout-node) "horizontal")
          ratio   (:ratio layout-node 0.5)]
      [:div.iris-command-center-split
       {:class (if horiz? "horizontal" "vertical")}
       [:div {:style {:flex (str ratio)}}
        [command-center-mini-layout c1 entities on-close-entity source-pos]]
       [:div {:style {:flex (str (- 1 ratio))}}
        [command-center-mini-layout c2 entities on-close-entity source-pos]]])

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
                           pos on-active-position-change command-center?)))}
     (when has-layout?
       [command-center-mini-layout (:layout workspace) entities
        (cell-on-close-tile pos workspace workspaces on-workspaces-change)
        pos])]))

(defn workspace-grid
  "Render the grid of all workspace cells."
  [cols rows workspaces active-position entities on-workspaces-change on-active-position-change command-center?]
  (let [view-cols (+ cols 1)
        view-rows (+ rows 1)
        [ax ay]   active-position]
    [dnd-context {:on-drag-start (on-drag-start-handler)
                  :on-drag-end   (on-drag-end-handler workspaces on-workspaces-change)}
     [:div.iris-command-center-grid
      {:style {:grid-template-columns (str "repeat(" view-cols ", 1fr)")
               :grid-template-rows    (str "repeat(" view-rows ", 1fr)")}}
      (doall
        (for [y (range view-rows)
              x (range view-cols)]
          (let [pos [x y]]
            ^{:key (str x "," y)}
            [grid-cell-fc {:pos                       pos
                            :workspace                 (get workspaces pos)
                            :workspaces                workspaces
                            :active-pos                [ax ay]
                            :entities                  entities
                            :on-workspaces-change      on-workspaces-change
                            :on-active-position-change on-active-position-change
                            :command-center?           command-center?}]))]]))
