(ns iris-layout.workspace.command-center
  "Command Center overlay — mini workspace previews and entity palette."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]))

;; Drag state: {:tile-id "..." :entity-id "..." :source-pos [x y]}
(def picked-tile (r/atom nil))
(def pending-drag (atom nil))
(def drag-threshold 5)

(defn start-drag-listeners!
  "Attach temporary mousemove/mouseup listeners to detect a drag gesture in the command center."
  []
  (let [on-move (atom nil)
        on-up (atom nil)]
    (reset! on-move
      (fn [e]
        (when-let [pd @pending-drag]
          (let [dx (- (.-clientX e) (:start-x pd))
                dy (- (.-clientY e) (:start-y pd))]
            (when (> (+ (js/Math.abs dx) (js/Math.abs dy)) drag-threshold)
              (reset! picked-tile (select-keys pd [:tile-id :entity-id :source-pos]))
              (reset! pending-drag nil))))))
    (reset! on-up
      (fn [_]
        (reset! pending-drag nil)
        (js/setTimeout #(when @picked-tile (reset! picked-tile nil)) 50)
        (.removeEventListener js/document "mousemove" @on-move)
        (.removeEventListener js/document "mouseup" @on-up)))
    (.addEventListener js/document "mousemove" @on-move)
    (.addEventListener js/document "mouseup" @on-up)))

(defn command-center-mini-layout
  "Renders a miniature read-only preview of a layout tree inside the command center."
  [layout-node entities on-close-entity source-pos]
  (case (layout/node-type layout-node)
    :tile
    (let [entity  (get entities (keyword (:entity-id layout-node)))
          picked? (and @picked-tile
                       (= (:tile-id @picked-tile) (:id layout-node))
                       (= (:source-pos @picked-tile) source-pos))]
      [:div.iris-command-center-tile
       {:style (merge
                 (when (:color entity) {"--iris-tile-color" (:color entity)})
                 (when picked? {:outline "2px solid #fbbf24"
                                :outline-offset "-2px"
                                :opacity "0.5"}))
        :on-mouse-down (fn [e]
                         (.stopPropagation e)
                         (.preventDefault e)
                         (reset! pending-drag {:tile-id    (:id layout-node)
                                               :entity-id  (:entity-id layout-node)
                                               :source-pos source-pos
                                               :start-x    (.-clientX e)
                                               :start-y    (.-clientY e)})
                         (start-drag-listeners!))}
       [:span.iris-command-center-tile-name (:name entity)]
       [:button.iris-command-center-tile-close
        {:on-click     (fn [e] (.stopPropagation e) (on-close-entity (:id layout-node)))
         :on-mouse-down (fn [e] (.stopPropagation e))}
        "\u00d7"]])

    :split
    (let [[c1 c2] (:children layout-node)
          horiz?  (= (:direction layout-node) "horizontal")
          ratio   (:ratio layout-node 0.5)]
      [:div.iris-command-center-split
       {:class (if horiz? "horizontal" "vertical")}
       [:div {:style {:flex (str ratio)}}
        [command-center-mini-layout c1 entities on-close-entity source-pos]]
       [:div {:style {:flex (str (- 1 ratio))}}
        [command-center-mini-layout c2 entities on-close-entity source-pos]]])

    nil))

(defn command-center-overlay
  "Full-screen overlay showing all workspaces as a mini grid plus an entity palette.
   Opens on Alt key hold or corner button click."
  [_cols _rows _workspaces _active-position _entities _render-entity-tile
   _on-active-position-change _on-workspaces-change _command-center?]
  (fn [cols rows workspaces active-position entities _render-entity-tile
       on-active-position-change on-workspaces-change command-center?]
      (let [[ax ay] active-position
            view-cols (+ cols 1)
            view-rows (+ rows 1)
            picked    @picked-tile]
        [:div {:class (str "iris-command-center"
                           (when @command-center? " iris-command-center-open"))
               :style (when picked {:cursor "grabbing"})}
         [:div.iris-command-center-backdrop
          {:on-click (fn [_] (when-not picked (reset! command-center? false)))}]
         [:div.iris-command-center-content
          [:div.iris-command-center-grid
           {:style {:grid-template-columns (str "repeat(" view-cols ", 1fr)")
                    :grid-template-rows    (str "repeat(" view-rows ", 1fr)")}}
           (doall
             (for [y (range view-rows)
                   x (range view-cols)]
               (let [pos          [x y]
                     workspace    (get workspaces pos)
                     active?      (and (= x ax) (= y ay))
                     has-layout?  (some? (:layout workspace))
                     drop-target? (and picked (not= (:source-pos picked) pos))]
                 ^{:key (str x "," y)}
                 [:div {:class (str "iris-command-center-cell"
                                    (when active?      " iris-command-center-cell-active")
                                    (when drop-target? " iris-command-center-cell-drop-target"))
                        :on-mouse-up
                        (fn [e]
                          (.stopPropagation e)
                          (when (and picked (not= (:source-pos picked) pos))
                            (let [{:keys [tile-id entity-id source-pos]} picked
                                  source-ws     (get workspaces source-pos)
                                  source-layout (layout/remove-tile-from-layout (:layout source-ws) tile-id)
                                  target-ws     (or workspace {:layout nil})
                                  new-tile      {:type :tile :id (util/generate-id) :entity-id entity-id}
                                  target-layout (if (:layout target-ws)
                                                  {:type :split :id (util/generate-id)
                                                   :direction "horizontal" :ratio 0.5
                                                   :children [(:layout target-ws) new-tile]}
                                                  new-tile)
                                  cleaned  (if source-layout
                                             (assoc workspaces source-pos (assoc source-ws :layout source-layout))
                                             (dissoc workspaces source-pos))
                                  updated  (assoc cleaned pos (assoc target-ws :layout target-layout))]
                              (on-workspaces-change updated)
                              (reset! picked-tile nil))))
                        :on-click (fn [e]
                                    (.stopPropagation e)
                                    (when-not picked (reset! command-center? false)))}
                  (when has-layout?
                    [command-center-mini-layout (:layout workspace) entities
                     (fn [tile-id]
                       (when on-workspaces-change
                         (let [new-layout (layout/remove-tile-from-layout (:layout workspace) tile-id)]
                           (on-workspaces-change
                             (if new-layout
                               (assoc workspaces pos (assoc workspace :layout new-layout))
                               (dissoc workspaces pos))))))
                     pos])])))]
          [:div.iris-command-center-palette
           (doall
             (for [[entity-id entity] entities
                   :when (not (:instance entity))]
               ^{:key entity-id}
               [:div.iris-command-center-palette-item
                {:style {"--iris-tile-color" (or (:color entity) "#6366f1")}
                 :on-click
                 (fn [e]
                   (.stopPropagation e)
                   (when on-workspaces-change
                     (let [active-pos [ax ay]
                           active-ws  (get workspaces active-pos)
                           existing   (:layout active-ws)
                           new-tile   {:type :tile :id (util/generate-id) :entity-id (name entity-id)}
                           new-layout (if existing
                                        {:type :split :id (util/generate-id)
                                         :direction "horizontal" :ratio 0.5
                                         :children [existing new-tile]}
                                        new-tile)]
                       (on-workspaces-change
                         (assoc workspaces active-pos (assoc active-ws :layout new-layout))))))}
                (if-let [icon (:icon entity)]
                  [:div.iris-command-center-palette-icon [icon]]
                  [:div.iris-command-center-palette-dot
                   {:style {:background (or (:color entity) "#6366f1")}}])
                [:span.iris-command-center-palette-name (:name entity)]]))]]])))
