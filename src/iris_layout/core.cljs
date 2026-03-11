(ns iris-layout.core
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.components.surface :as surface]))

;; --- JS <-> CLJS boundary conversion ---

(defn- generate-id []
  (str "iris-" (random-uuid)))

(defn js->layout
  "Convert a JS layout object to a CLJS layout map"
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
  "Convert a CLJS layout map to a JS object"
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
   Keeps top-level keys as strings (entity IDs) but keywordizes nested keys."
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
  "Convert a JS stages array to CLJS"
  [js-arr]
  (when js-arr
    (mapv (fn [s]
            (let [obj (js->clj s)]
              {:id    (get obj "id")
               :label (get obj "label")
               :layout (js->layout (get obj "layout"))}))
          js-arr)))

(defn stages->js
  "Convert CLJS stages to JS"
  [stages]
  (clj->js
    (mapv (fn [s]
            {"id"     (:id s)
             "label"  (:label s)
             "layout" (layout->js (:layout s))})
          stages)))

;; --- Reagent components (for CLJS consumers) ---

(defn stage-component
  "A single layout stage. Props map:
   :layout        - layout tree
   :entities      - map of entity-id -> entity data
   :render-entity - reagent component fn [entity] -> hiccup
   :on-layout-change - fn(new-layout) called on layout mutations
   :focused-tile  - optional tile id"
  [_]
  (let [;; Stable refs that survive re-renders — callbacks always read latest props
        props-ref (atom nil)
        handle-split (fn [tile-id entity-id split-direction _before?]
                       (let [{:keys [layout on-layout-change]} @props-ref
                             new-tile-id (generate-id)
                             split-id (generate-id)
                             new-layout (layout/split-tile
                                          layout tile-id split-direction
                                          entity-id new-tile-id split-id)]
                         (when (and new-layout on-layout-change)
                           (on-layout-change new-layout))))
        handle-ratio (fn [split-id new-ratio]
                       (let [{:keys [layout on-layout-change]} @props-ref
                             new-layout (layout/update-split-ratio layout split-id new-ratio)]
                         (when on-layout-change
                           (on-layout-change new-layout))))]
    (fn [{:keys [layout entities render-entity on-layout-change focused-tile] :as props}]
      (reset! props-ref props)
      [:div.iris-stage
       [surface/surface layout handle-split handle-ratio focused-tile entities render-entity]])))

(defn stages-component
  "Multi-stage container. Props map:
   :stages              - vector of {:id :label :layout}
   :active-stage        - id of the active stage
   :entities            - map of entity-id -> entity data
   :render-entity       - reagent component fn [entity] -> hiccup
   :on-stages-change    - fn(new-stages) called on layout mutations
   :on-active-stage-change - fn(stage-id) called on stage switch"
  [{:keys [stages active-stage entities render-entity
           on-stages-change on-active-stage-change]}]
  (let [current-stage (or (some #(when (= (:id %) active-stage) %) stages)
                          (first stages))]
    [:div.iris-stages
     [:div.iris-stage-tabs
      (for [stage stages]
        ^{:key (:id stage)}
        [:button.iris-stage-tab
         {:class (when (= (:id stage) (:id current-stage)) "iris-stage-tab-active")
          :on-click #(when on-active-stage-change
                       (on-active-stage-change (:id stage)))}
         (:label stage)])]
     (when current-stage
       [stage-component
        {:layout (:layout current-stage)
         :entities entities
         :render-entity render-entity
         :focused-tile nil
         :on-layout-change
         (fn [new-layout]
           (when on-stages-change
             (let [updated (mapv (fn [s]
                                   (if (= (:id s) (:id current-stage))
                                     (assoc s :layout new-layout)
                                     s))
                                 stages)]
               (on-stages-change updated))))}])]))

;; --- React-facing wrappers (for JS consumers) ---
;; reactify-component auto-converts JS props to CLJS map with keyword keys.
;; Nested objects (layout, entities) remain as JS objects and need manual conversion.

(defn- stage-wrapper [{:keys [layout entities renderEntity onLayoutChange focusedTile]}]
  (let [clj-layout (js->layout layout)
        clj-entities (js->entities entities)]
    [stage-component
     {:layout clj-layout
      :entities clj-entities
      :render-entity renderEntity
      :on-layout-change (when onLayoutChange
                          (fn [new-layout]
                            (onLayoutChange (layout->js new-layout))))
      :focused-tile focusedTile}]))

(defn- stages-wrapper [{:keys [stages activeStage entities renderEntity
                                onStagesChange onActiveStageChange]}]
  (let [clj-stages (js->stages stages)
        clj-entities (js->entities entities)]
    [stages-component
     {:stages clj-stages
      :active-stage activeStage
      :entities clj-entities
      :render-entity renderEntity
      :on-stages-change (when onStagesChange
                          (fn [new-stages]
                            (onStagesChange (stages->js new-stages))))
      :on-active-stage-change onActiveStageChange}]))

;; --- Exported React components ---

(def Stage (r/reactify-component stage-wrapper))
(def Stages (r/reactify-component stages-wrapper))
