(ns iris-layout.command-center.start-menu
  "Start menu — entity palette for adding entities to the active workspace."
  (:require [clojure.string]
            [iris-layout.util :as util]
            [iris-layout.layout :as layout]
            [iris-layout.pane.tile :as tile]))

(defonce color-index (atom 0))

(defn next-color
  "Return the next color from the preset palette, cycling through them."
  []
  (let [colors tile/preset-colors
        idx    @color-index]
    (swap! color-index #(mod (inc %) (count colors)))
    (nth colors idx)))

(defn optimal-split-direction
  "Measure a DOM element and return the direction that halves the longer axis."
  [el]
  (if el
    (let [rect (.getBoundingClientRect el)]
      (if (>= (.-width rect) (.-height rect))
        "horizontal"
        "vertical"))
    "horizontal"))

(defn palette-item-on-click
  "Handle clicking an entity type in the palette — split the active tile, or wrap the layout if none."
  [entity-type active-pos workspaces on-workspaces-change on-entity-create active-entity on-active-entity-change]
  (fn [e]
    (.stopPropagation e)
    (let [instance-id (str (name (:type entity-type)) "-" (util/generate-id))
          active-ws   (get workspaces active-pos)
          existing    (:layout active-ws)
          active-tile (when active-entity
                        (layout/find-tile-by-entity existing active-entity))
          target-el   (when active-tile
                        (.querySelector js/document
                          (str "[data-tile-id=\"" (:id active-tile) "\"]")))
          direction   (optimal-split-direction
                        (or target-el
                            (.querySelector js/document ".iris-grid-cell-active")))
          new-layout  (if active-tile
                        (layout/split-tile existing (:id active-tile)
                          direction instance-id (util/generate-id) (util/generate-id))
                        (if existing
                          {:type :split :id (util/generate-id)
                           :direction direction :ratio 0.5
                           :children [existing {:type :tile :id (util/generate-id) :entity-id instance-id}]}
                          {:type :tile :id (util/generate-id) :entity-id instance-id}))]
      (when on-entity-create
        (on-entity-create {:instance-id instance-id
                           :type        (:type entity-type)
                           :name        (:name entity-type)
                           :color       (next-color)}))
      (when on-workspaces-change
        (on-workspaces-change
          (assoc workspaces active-pos (assoc active-ws :layout new-layout))))
      (when on-active-entity-change
        (on-active-entity-change instance-id)))))

(defn palette-item
  "Render a single entity type in the start menu palette."
  [entity-type active-pos workspaces on-workspaces-change on-entity-create active-entity on-active-entity-change]
  [:div.iris-command-center-palette-item
   {:style {"--iris-tile-color" (or (:color entity-type) "#6366f1")}
    :on-click (palette-item-on-click entity-type active-pos workspaces
                                     on-workspaces-change on-entity-create active-entity on-active-entity-change)}
   (if-let [icon (:icon entity-type)]
     [:div.iris-command-center-palette-icon [icon]]
     [:div.iris-command-center-palette-dot
      {:style {:background (or (:color entity-type) "#6366f1")}}])
   [:span.iris-command-center-palette-name (:name entity-type)]])

(defn start-menu-items
  [{:keys [entity-types active-position workspaces on-workspaces-change on-entity-create active-entity on-active-entity-change]}]
  [:<>
   (map (fn [et]
          ^{:key (or (:type et) (:id et))}
          [palette-item et active-position workspaces on-workspaces-change on-entity-create active-entity on-active-entity-change])
        (sort-by (comp clojure.string/lower-case :name) entity-types))])

(defn start-menu
  "Render the start menu with all available entity types."
  [props]
  [:div.iris-command-center-palette
   [start-menu-items props]])
