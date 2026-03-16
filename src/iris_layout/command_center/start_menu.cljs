(ns iris-layout.command-center.start-menu
  "Start menu — entity palette for adding entities to the active workspace."
  (:require [iris-layout.util :as util]))

(defn palette-item-on-click
  "Handle clicking an entity in the palette — add it to the active workspace."
  [entity-id active-pos workspaces on-workspaces-change]
  (fn [e]
    (.stopPropagation e)
    (when on-workspaces-change
      (let [active-ws  (get workspaces active-pos)
            existing   (:layout active-ws)
            new-tile   {:type :tile :id (util/generate-id) :entity-id (name entity-id)}
            new-layout (if existing
                         {:type :split :id (util/generate-id)
                          :direction "horizontal" :ratio 0.5
                          :children [existing new-tile]}
                         new-tile)]
        (on-workspaces-change
          (assoc workspaces active-pos (assoc active-ws :layout new-layout)))))))

(defn palette-item
  "Render a single entity in the start menu palette."
  [entity-id entity active-pos workspaces on-workspaces-change]
  ^{:key entity-id}
  [:div.iris-command-center-palette-item
   {:style {"--iris-tile-color" (or (:color entity) "#6366f1")}
    :on-click (palette-item-on-click entity-id active-pos workspaces on-workspaces-change)}
   (if-let [icon (:icon entity)]
     [:div.iris-command-center-palette-icon [icon]]
     [:div.iris-command-center-palette-dot
      {:style {:background (or (:color entity) "#6366f1")}}])
   [:span.iris-command-center-palette-name (:name entity)]])

(defn start-menu
  "Render the start menu with all available entities."
  [entities active-position workspaces on-workspaces-change]
  [:div.iris-command-center-palette
   (doall
     (for [[entity-id entity] entities
           :when (not (:instance entity))]
       [palette-item entity-id entity active-position workspaces on-workspaces-change]))])
