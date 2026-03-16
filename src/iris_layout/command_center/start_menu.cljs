(ns iris-layout.command-center.start-menu
  "Start menu — entity palette for adding entities to the active workspace."
  (:require [clojure.string]
            [iris-layout.util :as util]))

(defn palette-item-on-click
  "Handle clicking an entity type in the palette — create a new instance and add it."
  [entity-type active-pos workspaces on-workspaces-change on-entity-create]
  (fn [e]
    (.stopPropagation e)
    (let [instance-id (str (name (:type entity-type)) "-" (util/generate-id))
          active-ws   (get workspaces active-pos)
          existing    (:layout active-ws)
          new-tile    {:type :tile :id (util/generate-id) :entity-id instance-id}
          new-layout  (if existing
                        {:type :split :id (util/generate-id)
                         :direction "horizontal" :ratio 0.5
                         :children [existing new-tile]}
                        new-tile)]
      (when on-entity-create
        (on-entity-create {:instance-id instance-id
                           :type        (:type entity-type)
                           :name        (:name entity-type)
                           :color       (:color entity-type)}))
      (when on-workspaces-change
        (on-workspaces-change
          (assoc workspaces active-pos (assoc active-ws :layout new-layout)))))))

(defn palette-item
  "Render a single entity type in the start menu palette."
  [entity-type active-pos workspaces on-workspaces-change on-entity-create]
  [:div.iris-command-center-palette-item
   {:style {"--iris-tile-color" (or (:color entity-type) "#6366f1")}
    :on-click (palette-item-on-click entity-type active-pos workspaces
                                     on-workspaces-change on-entity-create)}
   (if-let [icon (:icon entity-type)]
     [:div.iris-command-center-palette-icon [icon]]
     [:div.iris-command-center-palette-dot
      {:style {:background (or (:color entity-type) "#6366f1")}}])
   [:span.iris-command-center-palette-name (:name entity-type)]])

(defn start-menu-items
  [{:keys [entity-types active-position workspaces on-workspaces-change on-entity-create]}]
  [:<>
   (map (fn [et]
          ^{:key (or (:type et) (:id et))}
          [palette-item et active-position workspaces on-workspaces-change on-entity-create])
        (sort-by (comp clojure.string/lower-case :name) entity-types))])

(defn start-menu
  "Render the start menu with all available entity types."
  [props]
  [:div.iris-command-center-palette
   [start-menu-items props]])
