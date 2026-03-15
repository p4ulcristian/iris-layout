(ns iris-layout.components.command-center
  "Command Center overlay — mini workspace previews and entity palette."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]))

(defn- pos-key
  [[x y]]
  (str x "," y))

(defn- command-center-mini-layout
  "Recursively render a mini layout tree with close buttons on each tile."
  [layout-node entities on-close-entity]
  (case (:type layout-node)
    :tile
    (let [entity (get entities (:entity-id layout-node))]
      [:div.iris-command-center-tile
       {:style (when (:color entity)
                 {"--iris-tile-color" (:color entity)})}
       [:span.iris-command-center-tile-name
        (or (:name entity) (:entity-id layout-node))]
       [:button.iris-command-center-tile-close
        {:on-click (fn [e]
                     (.stopPropagation e)
                     (on-close-entity (:entity-id layout-node)))}
        "\u00d7"]])

    :split
    (let [[c1 c2] (:children layout-node)
          horiz? (= (:direction layout-node) :horizontal)
          ratio (:ratio layout-node 0.5)]
      [:div.iris-command-center-split
       {:class (if horiz? "horizontal" "vertical")}
       [:div {:style {:flex (str ratio)}} [command-center-mini-layout c1 entities on-close-entity]]
       [:div {:style {:flex (str (- 1 ratio))}} [command-center-mini-layout c2 entities on-close-entity]]])

    nil))

(def ^:private hover-delay-ms 400)

(defn command-center-overlay
  "Overlay showing mini previews of all workspaces and entity palette.
   Appears when Alt is held."
  [cols rows workspaces active-position entities render-entity-tile
   on-active-position-change on-workspaces-change command-center?]
  (let [hover-timer (atom nil)
        active-key (pos-key active-position)
        view-cols (+ cols 1)
        view-rows (+ rows 1)]
    [:div.iris-command-center
     [:div.iris-command-center-backdrop
      {:on-click (fn [_] (reset! command-center? false))}]
     [:div.iris-command-center-content
      [:div.iris-command-center-grid
       {:style {:grid-template-columns (str "repeat(" view-cols ", 1fr)")
                :grid-template-rows (str "repeat(" view-rows ", 1fr)")}}
       (doall
         (for [y (range view-rows)
               x (range view-cols)]
           (let [k (pos-key [x y])
                 workspace (get workspaces k)
                 active? (= k active-key)
                 has-layout? (some? (:layout workspace))]
             ^{:key k}
             [:div {:class (str "iris-command-center-cell"
                                (when active? " iris-command-center-cell-active"))
                    :on-mouse-enter (when (not active?)
                                     (fn [_]
                                       (when @hover-timer (js/clearTimeout @hover-timer))
                                       (reset! hover-timer
                                         (js/setTimeout
                                           (fn [] (when on-active-position-change
                                                    (on-active-position-change [x y])))
                                           hover-delay-ms))))
                   :on-mouse-leave (fn [_]
                                     (when @hover-timer
                                       (js/clearTimeout @hover-timer)
                                       (reset! hover-timer nil)))
                    :on-click (fn [_]
                                (reset! command-center? false))}
              (when has-layout?
                [command-center-mini-layout (:layout workspace) entities
                 (fn [entity-id]
                   (when on-workspaces-change
                     (let [new-layout (layout/remove-entity-from-layout (:layout workspace) entity-id)]
                       (on-workspaces-change
                         (assoc workspaces k {:layout new-layout})))))])])))]
      [:div.iris-command-center-palette
       (doall
         (for [[entity-id entity] entities]
           ^{:key entity-id}
           [:div.iris-command-center-palette-item
            {:style {"--iris-tile-color" (or (:color entity) "#6366f1")}
             :on-click (fn [e]
                         (.stopPropagation e)
                         (when on-workspaces-change
                           (let [active-key (pos-key active-position)
                                 existing-layout (:layout (get workspaces active-key))
                                 new-tile {:type :tile :id (str "iris-" (random-uuid)) :entity-id entity-id}
                                 new-layout (if existing-layout
                                              {:type :split :id (str "iris-" (random-uuid))
                                               :direction :horizontal :ratio 0.5
                                               :children [existing-layout new-tile]}
                                              new-tile)]
                             (on-workspaces-change (assoc workspaces active-key {:layout new-layout})))))}
            [:div.iris-command-center-palette-dot
             {:style {:background (or (:color entity) "#6366f1")}}]
            [:span.iris-command-center-palette-name
             (or (:name entity) entity-id)]]))]]]))
