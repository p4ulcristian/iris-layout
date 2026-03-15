(ns iris-layout.components.command-center
  "Command Center overlay — mini workspace previews and entity palette."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]))

(defn- pos-key
  [[x y]]
  (str x "," y))

;; Drag state: {:entity-id "..." :source-ws-key "0,0"}
;; Set only after mouse moves past threshold.
(def ^:private picked-tile (r/atom nil))

;; Pending drag — stored on mousedown, promoted to picked-tile on mousemove.
;; Plain atom so it doesn't cause re-renders.
(def ^:private pending-drag (atom nil))

(def ^:private drag-threshold 5)

(defn- start-drag-listeners! []
  (let [on-move (atom nil)
        on-up (atom nil)]
    (reset! on-move
      (fn [e]
        (when-let [pd @pending-drag]
          (let [dx (- (.-clientX e) (:start-x pd))
                dy (- (.-clientY e) (:start-y pd))]
            (when (> (+ (js/Math.abs dx) (js/Math.abs dy)) drag-threshold)
              (reset! picked-tile (select-keys pd [:tile-id :entity-id :source-ws-key]))
              (reset! pending-drag nil))))))
    (reset! on-up
      (fn [_]
        (reset! pending-drag nil)
        (js/setTimeout #(when @picked-tile (reset! picked-tile nil)) 50)
        (.removeEventListener js/document "mousemove" @on-move)
        (.removeEventListener js/document "mouseup" @on-up)))
    (.addEventListener js/document "mousemove" @on-move)
    (.addEventListener js/document "mouseup" @on-up)))

(defn- command-center-mini-layout
  "Recursively render a mini layout tree with close buttons on each tile.
   Drag a tile to move it to another workspace."
  [layout-node entities on-close-entity source-ws-key]
  (case (layout/node-type layout-node)
    :tile
    (let [entity (get entities (keyword (:entity-id layout-node)))
          picked? (and @picked-tile
                       (= (:tile-id @picked-tile) (:id layout-node))
                       (= (:source-ws-key @picked-tile) source-ws-key))]
      [:div.iris-command-center-tile
       {:style (merge
                 (when (:color entity) {"--iris-tile-color" (:color entity)})
                 (when picked? {:outline "2px solid #fbbf24"
                                :outline-offset "-2px"
                                :opacity "0.5"}))
        :on-mouse-down (fn [e]
                         (.stopPropagation e)
                         (.preventDefault e)
                         (reset! pending-drag {:tile-id (:id layout-node)
                                              :entity-id (:entity-id layout-node)
                                              :source-ws-key source-ws-key
                                              :start-x (.-clientX e)
                                              :start-y (.-clientY e)})
                         (start-drag-listeners!))}
       [:span.iris-command-center-tile-name
        (:name entity)]
       [:button.iris-command-center-tile-close
        {:on-click (fn [e]
                     (.stopPropagation e)
                     (on-close-entity (:id layout-node)))
         :on-mouse-down (fn [e] (.stopPropagation e))}
        "\u00d7"]])

    :split
    (let [[c1 c2] (:children layout-node)
          horiz? (= (keyword (:direction layout-node)) :horizontal)
          ratio (:ratio layout-node 0.5)]
      [:div.iris-command-center-split
       {:class (if horiz? "horizontal" "vertical")}
       [:div {:style {:flex (str ratio)}} [command-center-mini-layout c1 entities on-close-entity source-ws-key]]
       [:div {:style {:flex (str (- 1 ratio))}} [command-center-mini-layout c2 entities on-close-entity source-ws-key]]])

    nil))

(def ^:private hover-delay-ms 400)

(defn command-center-overlay
  "Overlay showing mini previews of all workspaces and entity palette.
   Appears when Alt is held."
  [_cols _rows _workspaces _active-position _entities _render-entity-tile
   _on-active-position-change _on-workspaces-change _command-center?]
  (let [hover-timer (atom nil)]
    (fn [cols rows workspaces active-position entities render-entity-tile
         on-active-position-change on-workspaces-change command-center?]
      (let [active-key (pos-key active-position)
            view-cols (+ cols 1)
            view-rows (+ rows 1)
            picked @picked-tile]
        [:div {:class (str "iris-command-center"
                          (when @command-center? " iris-command-center-open"))
               :style (when picked {:cursor "grabbing"})}
         [:div.iris-command-center-backdrop
          {:on-click (fn [_]
                       (when-not picked
                         (reset! command-center? false)))}]
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
                     has-layout? (some? (:layout workspace))
                     is-drop-target? (and picked (not= (:source-ws-key picked) k))]
                 ^{:key k}
                 [:div {:class (str "iris-command-center-cell"
                                    (when active? " iris-command-center-cell-active")
                                    (when is-drop-target? " iris-command-center-cell-drop-target"))
                        :on-mouse-enter (when (not active?)
                                          (fn [_]
                                            (when-not picked
                                              (when @hover-timer (js/clearTimeout @hover-timer))
                                              (reset! hover-timer
                                                (js/setTimeout
                                                  (fn [] (when on-active-position-change
                                                           (on-active-position-change [x y])))
                                                  hover-delay-ms)))))
                        :on-mouse-leave (fn [_]
                                          (when @hover-timer
                                            (js/clearTimeout @hover-timer)
                                            (reset! hover-timer nil)))
                        :on-mouse-up (fn [e]
                                       (.stopPropagation e)
                                       (when (and picked (not= (:source-ws-key picked) k))
                                         (let [{:keys [tile-id entity-id source-ws-key]} picked
                                               source-ws (get workspaces source-ws-key)
                                               source-layout (layout/remove-tile-from-layout (:layout source-ws) tile-id)
                                               target-ws (get workspaces k)
                                               new-tile {:type :tile :id (util/generate-id) :entity-id entity-id}
                                               target-layout (if (:layout target-ws)
                                                               {:type :split :id (util/generate-id)
                                                                :direction :horizontal :ratio 0.5
                                                                :children [(:layout target-ws) new-tile]}
                                                               new-tile)
                                               cleaned (if source-layout
                                                          (assoc workspaces source-ws-key {:layout source-layout})
                                                          (dissoc workspaces source-ws-key))
                                               updated (assoc cleaned k {:layout target-layout})]
                                           (on-workspaces-change updated)
                                           (reset! picked-tile nil))))
                        :on-click (fn [e]
                                    (.stopPropagation e)
                                    (when-not picked
                                      (reset! command-center? false)))}
                  (when has-layout?
                    [command-center-mini-layout (:layout workspace) entities
                     (fn [tile-id]
                       (when on-workspaces-change
                         (let [new-layout (layout/remove-tile-from-layout (:layout workspace) tile-id)]
                           (on-workspaces-change
                             (if new-layout
                               (assoc workspaces k {:layout new-layout})
                               (dissoc workspaces k))))))
                     k])])))]
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
                     (let [ak (pos-key active-position)
                           existing (:layout (get workspaces ak))
                           new-tile {:type :tile :id (util/generate-id) :entity-id (name entity-id)}
                           new-layout (if existing
                                        {:type :split :id (util/generate-id)
                                         :direction :horizontal :ratio 0.5
                                         :children [existing new-tile]}
                                        new-tile)]
                       (on-workspaces-change (assoc workspaces ak {:layout new-layout})))))}
                (if-let [icon (:icon entity)]
                  [:div.iris-command-center-palette-icon [:> icon {}]]
                  [:div.iris-command-center-palette-dot
                   {:style {:background (or (:color entity) "#6366f1")}}])
                [:span.iris-command-center-palette-name
                 (:name entity)]]))]]]))))
