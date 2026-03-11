(ns iris-layout.components.entity-card-group
  "Entity card group — a single stage card for the Sidebar.

   Renders a clickable card showing:
   - Stage label
   - List of entity cards (rendered via `render-entity-card`)

   Each entity card is draggable onto Body tiles to add entities to a layout.
   The drag data format is JSON: {entityId: string, source: \"sidebar\"}."
  (:require [iris-layout.layout :as layout]))

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
   :on-entity-close    - fn(entity-id) called when an entity card's close button is clicked"
  [{:keys [stage active? entities active-entity render-entity-card on-click on-entity-click on-entity-close]}]
  (let [entity-ids (layout/collect-entity-ids (:layout stage))]
    [:div.iris-entity-card-group
     {:class (when active? "iris-entity-card-group-active")
      :on-click (fn [_] (when on-click (on-click)))}
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
               "×"]]))])]]))
