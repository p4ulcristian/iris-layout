(ns iris-layout.components.sidebar
  (:require [iris-layout.layout :as layout]))

(defn sidebar-component
  "Sidebar with stage cards containing draggable entity chips.
   Props:
   :title            - header text
   :stages           - vector of stage maps
   :active-stage     - active stage id
   :entities         - map of entity-id -> entity data (must have :name, :color)
   :on-active-stage-change - fn(stage-id)
   :on-stages-change - fn(new-stages)"
  [{:keys [title stages active-stage entities on-active-stage-change on-stages-change]}]
  [:div.iris-sidebar
   (when title
     [:div.iris-sidebar-title title])
   [:div.iris-sidebar-section
    [:div.iris-sidebar-heading "Stages"]
    [:div.iris-stage-list
     (for [stage stages]
       (let [entity-ids (layout/collect-entity-ids (:layout stage))
             active? (= (:id stage) active-stage)]
         ^{:key (:id stage)}
         [:div.iris-stage-card
          {:class (when active? "iris-stage-card-active")
           :on-click #(when on-active-stage-change
                        (on-active-stage-change (:id stage)))}
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
                                     (set! (.-effectAllowed (.-dataTransfer e)) "move"))}
                   [:span.iris-entity-dot {:style {:background (:color ent)}}]
                   (:name ent)]))])]]))]]])
