(ns iris-layout.command-center.core
  "Command Center — single element that is both trigger and overlay.
   Clipped to a quarter circle when closed, grows to full screen when open."
  (:require [iris-layout.command-center.mini-workspaces :as mini-workspaces]
            [iris-layout.command-center.start-menu :as start-menu]))

(defn default-logo []
  [:svg {:viewBox "0 0 24 24" :width "20" :height "20"}
   [:circle {:cx 12 :cy 12 :r 7 :fill "none" :stroke "currentColor" :stroke-width 1.5}]
   [:circle {:cx 12 :cy 12 :r 2.5 :fill "currentColor"}]])

(defn command-center
  "Single element: quarter-circle trigger when closed, full-screen overlay when open."
  [{:keys [cols rows workspaces active-position active-entity entities entity-types
           on-active-position-change on-workspaces-change on-entity-create
           on-active-entity-change command-center? logo]}]
  (let [open? @command-center?]
    [:div {:class (str "iris-command-center"
                       (when open? " iris-command-center-open"))
           :style (when @mini-workspaces/dragging? {:cursor "grabbing"})
           :on-click (fn [e]
                       (when (and open?
                                  (= (.-target e) (.-currentTarget e))
                                  (not @mini-workspaces/dragging?))
                         (reset! command-center? false)))}
     ;; Trigger hit area — clickable circle at bottom-left
     [:div.iris-command-center-trigger
      {:on-click (fn [e]
                   (.stopPropagation e)
                   (reset! command-center? (not open?)))}]
     ;; Content — only meaningful when open
     (when open?
       [:div.iris-command-center-content
        {:on-click (fn [e]
                     (when (and (= (.-target e) (.-currentTarget e))
                                (not @mini-workspaces/dragging?))
                       (reset! command-center? false)))}
        [mini-workspaces/workspace-grid
         {:cols                    cols
          :rows                    rows
          :workspaces              workspaces
          :active-position         active-position
          :entities                entities
          :on-workspaces-change    on-workspaces-change
          :on-active-position-change on-active-position-change
          :command-center?         command-center?}]
        [start-menu/start-menu {:entity-types             entity-types
                                :active-position          active-position
                                :active-entity            active-entity
                                :workspaces               workspaces
                                :on-workspaces-change     on-workspaces-change
                                :on-entity-create         on-entity-create
                                :on-active-entity-change  on-active-entity-change}]])]))
