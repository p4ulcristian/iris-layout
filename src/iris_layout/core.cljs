(ns iris-layout.core
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.components.surface :as surface]
            [iris-layout.components.sidebar :as sidebar]))

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
    (fn [{:keys [layout entities render-entity on-layout-change focused-tile] :as props}]
      (reset! props-ref props)
      [:div.iris-stage
       [surface/surface layout handle-split handle-ratio focused-tile entities render-entity]])))

(defn- stage-index
  "Find index of stage by id"
  [stages id]
  (first (keep-indexed (fn [i s] (when (= (:id s) id) i)) stages)))

(defn- restart-animation!
  "Force restart CSS animation on an element"
  [^js el anim-name]
  (when el
    (set! (.-animation (.-style el)) "none")
    ;; Force reflow by reading a layout property
    (.-offsetHeight el)
    (set! (.-animation (.-style el)) (str anim-name " 400ms cubic-bezier(0.22, 1, 0.36, 1)"))))

(defn stages-component
  "Multi-stage container — all stages rendered and stacked,
   active one slides into view over the others."
  [_]
  (let [prev-stage-id (atom nil)
        slide-dir (atom "right")
        ;; Store refs to stage layer DOM elements
        layer-refs (atom {})]
    (fn [{:keys [stages active-stage entities render-entity
                 on-stages-change on-active-stage-change]}]
      (let [prev-idx (stage-index stages @prev-stage-id)
            curr-idx (stage-index stages active-stage)
            changed? (and @prev-stage-id (not= @prev-stage-id active-stage))]
        ;; Determine slide direction
        (when changed?
          (reset! slide-dir (if (and prev-idx curr-idx (> curr-idx prev-idx))
                              "down" "up")))
        ;; Trigger animation after Reagent finishes rendering the DOM
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
        [:div.iris-stages
         [:div.iris-stage-tabs
          (for [stage stages]
            ^{:key (:id stage)}
            [:button.iris-stage-tab
             {:class (when (= (:id stage) active-stage) "iris-stage-tab-active")
              :on-click #(when on-active-stage-change
                           (on-active-stage-change (:id stage)))}
             (:label stage)])]
         ;; All stages stacked — each is always rendered
         [:div.iris-stages-stack
          (for [stage stages]
            ^{:key (:id stage)}
            [:div {:class (str "iris-stage-layer"
                               (when (= (:id stage) active-stage) " iris-stage-active"))
                   :ref (fn [el] (when el (swap! layer-refs assoc (:id stage) el)))}
             [stage-component
              {:layout (:layout stage)
               :entities entities
               :render-entity render-entity
               :focused-tile nil
               :on-layout-change
               (fn [new-layout]
                 (when on-stages-change
                   (let [updated (mapv (fn [s]
                                         (if (= (:id s) (:id stage))
                                           (assoc s :layout new-layout)
                                           s))
                                       stages)]
                     (on-stages-change updated))))}]])]]))))

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

(defn- sidebar-wrapper [{:keys [title stages activeStage entities
                                onActiveStageChange onStagesChange]}]
  (let [clj-stages (js->stages stages)
        clj-entities (js->entities entities)]
    [sidebar/sidebar-component
     {:title title
      :stages clj-stages
      :active-stage activeStage
      :entities clj-entities
      :on-active-stage-change onActiveStageChange
      :on-stages-change (when onStagesChange
                          (fn [new-stages]
                            (onStagesChange (stages->js new-stages))))}]))

;; --- Exported React components ---

(def Stage (r/reactify-component stage-wrapper))
(def Stages (r/reactify-component stages-wrapper))
(def Sidebar (r/reactify-component sidebar-wrapper))
