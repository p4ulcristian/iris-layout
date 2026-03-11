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
        (get obj "entityId")  (assoc :entity-id (get obj "entityId"))))))

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
        handle-split (fn [tile-id entity-id split-direction source-type]
                       (let [{:keys [layout on-layout-change]} @props-ref
                             target-tile (layout/find-tile layout tile-id)
                             same-tile? (and target-tile
                                             (= (:entity-id target-tile) entity-id))]
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
                                                entity-id new-tile-id split-id))]
                             (when (and new-layout on-layout-change)
                               (on-layout-change new-layout))))))
        handle-ratio (fn [split-id new-ratio]
                       (let [{:keys [layout on-layout-change]} @props-ref
                             new-layout (layout/update-split-ratio layout split-id new-ratio)]
                         (when on-layout-change
                           (on-layout-change new-layout))))]
    (fn [{:keys [layout entities render-entity-tile active-entity on-layout-change] :as props}]
      (reset! props-ref props)
      [:div.iris-body-stage
       [entity-tile-group/entity-tile-group
        layout handle-split handle-ratio active-entity entities render-entity-tile]])))

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
    (set! (.-animation (.-style el)) (str anim-name " 400ms cubic-bezier(0.22, 1, 0.36, 1)"))))

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
                 on-stages-change on-active-stage-change on-active-entity-change]}]
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
                     (on-stages-change updated))))}]])]]))))

;; ============================================================
;; Sidebar — stage cards with entity cards
;; ============================================================

(defn sidebar-component
  "Sidebar — renders a list of stage cards.

   Each card shows the stage label and its entities as draggable cards.
   Clicking a card switches the active stage.
   Clicking an entity card sets the active entity.
   Clicking an entity card's close button removes it from the stage layout.

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
  [{:keys [stages active-stage active-entity entities render-entity-card
           on-stages-change on-active-stage-change on-active-entity-change
           on-entity-close]}]
  [:div.iris-sidebar
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
         ;; Remove entity from this stage's layout
         ;; If layout becomes nil (empty), remove the entire stage
         (when on-stages-change
           (let [new-layout (layout/remove-entity-from-layout (:layout stage) entity-id)
                 updated (if (nil? new-layout)
                           (vec (remove #(= (:id %) (:id stage)) stages))
                           (mapv (fn [s]
                                   (if (= (:id s) (:id stage))
                                     (assoc s :layout new-layout)
                                     s))
                                 stages))]
             (on-stages-change updated)
             ;; If active stage was removed, switch to first remaining
             (when (and (nil? new-layout)
                        (= active-stage (:id stage))
                        on-active-stage-change
                        (seq updated))
               (on-active-stage-change (:id (first updated))))))
         ;; Optional side-effect callback
         (when on-entity-close
           (on-entity-close (:id stage) entity-id)))}])])

;; ============================================================
;; React-facing wrappers (JS consumers)
;; ============================================================
;; reactify-component auto-converts JS props to CLJS map with keyword keys.
;; Nested objects (layout, entities, stages) remain as JS and need manual conversion.

(defn- body-wrapper
  "Wrapper that converts JS props to CLJS for body-component."
  [{:keys [stages activeStage activeEntity entities renderEntityTile
           onStagesChange onActiveStageChange onActiveEntityChange]}]
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
    :on-active-entity-change onActiveEntityChange}])

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
