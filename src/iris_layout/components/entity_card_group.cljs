(ns iris-layout.components.entity-card-group
  "Entity card group — a single stage card for the Sidebar.

   Renders a clickable card showing:
   - Stage label
   - List of entity cards (rendered via `render-entity-card`)

   Each entity card is draggable onto Body tiles or other groups.
   The drag data format is JSON: {entityId: string, stageId: string, source: \"sidebar\"}.

   Groups are also drop targets — drop an entity card on a group to move it there."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]))

(defn entity-card-group-component
  "Render a single stage as a card in the sidebar.

   Props:
   :stage              - stage map {:id :label :layout}
   :active?            - boolean, whether this is the active stage
   :entities           - map of entity-id → entity data
   :active-entity      - entity-id of the focused entity (or nil)
   :render-entity-card - React component fn, receives entity data as props
   :on-click           - fn() called when the card is clicked
   :on-entity-click    - fn(entity-id) called when an entity card is clicked
   :on-entity-close    - fn(entity-id) called when close button is clicked
   :on-entity-drop     - fn(entity-id, source-stage-id) called when an entity is dropped on this group"
  [_]
  (let [drag-over (r/atom false)]
    (fn [{:keys [stage active? entities active-entity render-entity-card
                 on-click on-entity-click on-entity-close on-entity-drop]}]
      (let [entity-ids (layout/collect-entity-ids (:layout stage))]
        [:div.iris-entity-card-group
         {:class (str (when active? "iris-entity-card-group-active")
                      (when @drag-over " iris-entity-card-group-drag-over"))
          :on-click (fn [_] (when on-click (on-click)))
          :on-drag-over (fn [e]
                          (.preventDefault e)
                          (reset! drag-over true))
          :on-drag-enter (fn [e] (.preventDefault e))
          :on-drag-leave (fn [e]
                           (when (not (.contains (.-currentTarget e) (.-relatedTarget e)))
                             (reset! drag-over false)))
          :on-drop (fn [e]
                     (.preventDefault e)
                     (.stopPropagation e)
                     (reset! drag-over false)
                     (let [raw (.getData (.-dataTransfer e) "text/plain")]
                       (try
                         (let [data (js/JSON.parse raw)]
                           (when (and (= (.-source data) "sidebar")
                                      (.-entityId data)
                                      on-entity-drop)
                             ;; Don't drop on same stage
                             (when (not= (.-stageId data) (:id stage))
                               (on-entity-drop (.-entityId data) (.-stageId data)))))
                         (catch :default _ nil))))}
         [:div.iris-entity-card-group-content
          [:span.iris-entity-card-group-label (:label stage)]
          (when (seq entity-ids)
            [:div.iris-entity-card-group-entities
             (for [eid entity-ids]
               (when-let [ent (get entities eid)]
                 ^{:key eid}
                 [:div.iris-entity-card
                  {:class (when (= eid active-entity) "iris-entity-card-active")
                   :draggable true
                   :on-drag-start (fn [e]
                                    (.stopPropagation e)
                                    (.setData (.-dataTransfer e) "text/plain"
                                              (js/JSON.stringify #js {:entityId eid
                                                                      :stageId (:id stage)
                                                                      :source "sidebar"}))
                                    (set! (.-effectAllowed (.-dataTransfer e)) "all"))
                   :on-click (fn [e]
                               (.stopPropagation e)
                               (when on-entity-click (on-entity-click eid)))}
                  [:> render-entity-card ent]
                  [:button.iris-entity-card-close
                   {:on-click (fn [e]
                                (.stopPropagation e)
                                (when on-entity-close (on-entity-close eid)))}
                   "×"]]))])]]))))
