(ns iris-demo.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [iris-demo.state :as state]
            [iris-demo.layout :as layout]
            [iris-demo.components.surface :as surface]
            [iris-demo.components.entity :as entity]))

(defn handle-split
  "Handle splitting a tile when an entity is dropped"
  [tile-id entity-id split-direction before?]
  (let [current-layout (:layout @state/app-state)
        new-tile-id (state/get-next-id!)
        split-id (state/get-next-id!)
        direction split-direction
        new-layout (layout/split-tile
                    current-layout
                    tile-id
                    direction
                    entity-id
                    new-tile-id
                    split-id)]
    (when new-layout
      (swap! state/app-state assoc :layout new-layout)
      (swap! state/app-state assoc :focused-tile new-tile-id))))

(defn handle-ratio-change
  "Handle resizer drag to update split ratio"
  [split-id new-ratio]
  (let [current-layout (:layout @state/app-state)
        new-layout (layout/update-split-ratio current-layout split-id new-ratio)]
    (swap! state/app-state assoc :layout new-layout)))

(defn handle-drag-start
  "Handle drag start for entity items"
  [entity e]
  (let [data-transfer (.-dataTransfer e)]
    (.setData data-transfer "text/plain" (:id entity))
    (set! (.-effectAllowed data-transfer) "copy")))

(defn sidebar
  "Sidebar with draggable entity items"
  []
  (let [entities (:entities @state/app-state)]
    [:div.sidebar
     [:div.sidebar-title "Entities"]
     (for [[id entity-data] entities]
       ^{:key id}
       [entity/entity-item entity-data handle-drag-start])]))

(defn app
  "Main app component"
  []
  (let [layout (:layout @state/app-state)
        focused-tile (:focused-tile @state/app-state)
        entities (:entities @state/app-state)]
    [:div {:style {:display "flex" :height "100vh" :width "100vw"}}
     [sidebar]
     [:div.layout-root
      [surface/surface
       layout
       handle-split
       handle-ratio-change
       focused-tile
       entities]]]))

(defn mount-root
  "Mount the app to the DOM"
  []
  (rdom/render [app] (.getElementById js/document "app")))

(defn init!
  "Initialize the app"
  []
  (js/console.log "Iris Layout Demo initializing...")
  (mount-root))

(defn reload!
  "Hot reload handler"
  []
  (js/console.log "Reloading...")
  (mount-root))