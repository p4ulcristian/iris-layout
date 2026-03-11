(ns iris-demo.state
  (:require [reagent.core :as r]))

(def app-state
  (r/atom
   {:layout {:type :tile
             :id "tile-1"
             :entity-id "entity-1"}
    :entities {"entity-1" {:id "entity-1"
                           :type :browser
                           :name "Browser"
                           :url "https://example.com"
                           :color "#6366f1"}
               "entity-2" {:id "entity-2"
                           :type :notes
                           :name "Notes"
                           :color "#10b981"}
               "entity-3" {:id "entity-3"
                           :type :terminal
                           :name "Terminal"
                           :color "#f59e0b"}}
    :focused-tile "tile-1"
    :maximized-tile nil
    :next-id 2}))

(defn get-layout []
  (:layout @app-state))

(defn set-layout! [new-layout]
  (swap! app-state assoc :layout new-layout))

(defn get-entity [entity-id]
  (get-in @app-state [:entities entity-id]))

(defn get-focused-tile []
  (:focused-tile @app-state))

(defn set-focused-tile! [tile-id]
  (swap! app-state assoc :focused-tile tile-id))

(defn get-next-id! []
  (let [id (:next-id @app-state)]
    (swap! app-state update :next-id inc)
    (str "tile-" id)))