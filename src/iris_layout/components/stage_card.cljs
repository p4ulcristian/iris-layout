(ns iris-layout.components.stage-card
  (:require [iris-layout.layout :as layout]))

(defn stage-card-component
  "Card representing a single stage — shows label and draggable entity chips.
   Props:
   :stage     - stage map {:id :label :layout}
   :active?   - boolean
   :entities  - map of entity-id -> entity data (must have :name, :color)
   :on-click  - fn() called when card is clicked"
  [{:keys [stage active? entities on-click]}]
  (let [entity-ids (layout/collect-entity-ids (:layout stage))]
    [:div.iris-stage-card
     {:class (when active? "iris-stage-card-active")
      :on-click (fn [_] (when on-click (on-click)))}
     [:div.iris-stage-card-content
      [:span.iris-stage-card-label (:label stage)]
      (when (seq entity-ids)
        [:div.iris-stage-card-entities
         (for [eid entity-ids]
           (when-let [ent (get entities eid)]
             ^{:key eid}
             [:div.iris-entity-chip
              {:draggable true
               :on-drag-start (fn [e]
                                (.stopPropagation e)
                                (.setData (.-dataTransfer e) "text/plain"
                                          (js/JSON.stringify #js {:entityId eid
                                                                  :source "sidebar"}))
                                (set! (.-effectAllowed (.-dataTransfer e)) "all"))}
              [:span.iris-entity-dot {:style {:background (:color ent)}}]
              (:name ent)]))])]]))
