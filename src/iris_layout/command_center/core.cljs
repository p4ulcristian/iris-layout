(ns iris-layout.command-center.core
  "Command Center overlay — composes mini workspaces and start menu."
  (:require [iris-layout.command-center.mini-workspaces :as mini-workspaces]
            [iris-layout.command-center.start-menu :as start-menu]))

(defn command-center-overlay
  "Full-screen overlay showing all workspaces as a mini grid plus a start menu.
   Opens on Alt key hold or corner button click."
  [cols rows workspaces active-position entities _render-entity-tile
   on-active-position-change on-workspaces-change command-center?]
  [:div {:class (str "iris-command-center"
                     (when @command-center? " iris-command-center-open"))
         :style (when @mini-workspaces/dragging? {:cursor "grabbing"})}
   [:div.iris-command-center-backdrop
    {:on-click (fn [_] (when-not @mini-workspaces/dragging? (reset! command-center? false)))}]
   [:div.iris-command-center-content
    [mini-workspaces/workspace-grid cols rows workspaces active-position entities
     on-workspaces-change on-active-position-change command-center?]
    [start-menu/start-menu entities active-position workspaces on-workspaces-change]]])
