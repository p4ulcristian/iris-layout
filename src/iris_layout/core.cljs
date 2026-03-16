(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for Reagent."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.pane.pane :as pane]
            [iris-layout.pane.tile :as tile]
            [iris-layout.command-center.core :as command-center]
            [iris-layout.interactions.tile :as tile-interactions]
            [iris-layout.interactions.grid :as grid-interactions]))

(r/set-default-compiler! (r/create-compiler {:function-components true}))


;; ============================================================
;; Body stage — single workspace layout renderer
;; ============================================================

(defn- body-stage-component
  [{:keys [layout entities render-entity-tile active-entity
           on-layout-change on-active-entity-change on-entity-color-change on-entity-close]}]
  [:div.iris-body-stage
   [pane/pane
    layout
    (partial tile-interactions/handle-split layout on-layout-change)
    (partial tile-interactions/handle-close layout on-layout-change on-entity-close)
    (partial tile-interactions/handle-ratio layout on-layout-change)
    active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]])


;; ============================================================
;; Grid — 2D workspace grid with camera-based navigation
;; ============================================================

(defn- grid-dimensions
  "Compute grid bounds from workspace [x y] keys. Returns [cols rows]."
  [workspaces]
  (if (empty? workspaces)
    [1 1]
    [(inc (apply max (map first (keys workspaces))))
     (inc (apply max (map second (keys workspaces))))]))

(defn- nav-edge
  [{:keys [css-class visible? on-click direction props-ref nav-drag-edge]}]
  [:div {:class (str "iris-nav-edge " css-class
                     (when-not visible? " iris-nav-hidden")
                     (when (= @nav-drag-edge direction) " iris-nav-drag-over"))
         :on-click on-click
         :on-drag-over (fn [e] (.preventDefault e))
         :on-drag-enter (fn [e]
                          (.preventDefault e)
                          (reset! nav-drag-edge direction)
                          (grid-interactions/handle-grid-nav direction props-ref))
         :on-drag-leave (fn [e]
                          (when-not (.contains (.-currentTarget e) (.-relatedTarget e))
                            (reset! nav-drag-edge nil)))
         :on-drop (fn [e]
                    (.preventDefault e)
                    (.stopPropagation e)
                    (reset! nav-drag-edge nil))}
   [:div.iris-nav-semicircle]])


(defn- grid-cell
  [workspace x y active? props]
  (let [{:keys [entities render-entity-tile active-entity
                on-workspaces-change on-entity-close
                on-active-entity-change on-entity-color-change workspaces]} props
        pos [x y]]
    [:div {:class (str "iris-grid-cell"
                       (when active? " iris-grid-cell-active"))
           :data-position (str x "," y)}
     (if (:layout workspace)
       [body-stage-component
        {:layout (:layout workspace)
         :entities entities
         :render-entity-tile render-entity-tile
         :active-entity (when active? active-entity)
         :on-active-entity-change on-active-entity-change
         :on-entity-color-change on-entity-color-change
         :on-layout-change
         (fn [new-layout]
           (when on-workspaces-change
             (on-workspaces-change
               (update-workspaces-with-cleanup workspaces pos new-layout))))
         :on-entity-close on-entity-close}]
       [:div.iris-empty-workspace
        {:on-drag-over (fn [e] (.preventDefault e))
         :on-drop (fn [e]
                    (when on-workspaces-change
                      (grid-interactions/handle-empty-workspace-drop e x y workspaces on-workspaces-change @tile/dragging-tile)))}])]))

(defn- grid-canvas
  [cols rows workspaces active-position props]
  (let [[ax ay] active-position]
    (for [y (range rows)]
      (for [x (range cols)]
        ^{:key (str x "," y)}
        [grid-cell (get workspaces [x y]) x y (and (= x ax) (= y ay)) props]))))

(def ^:private grid-gap 16)

(defn- camera-style
  [cols rows active-position]
  (let [[ax ay] active-position
        tx (str "calc(" ax " * ((-100% - " grid-gap "px) / " cols "))")
        ty (str "calc(" ay " * ((-100% - " grid-gap "px) / " rows "))")]
    {:width  (str "calc(" cols " * 100% + " (* (dec cols) grid-gap) "px)")
     :height (str "calc(" rows " * 100% + " (* (dec rows) grid-gap) "px)")
     :grid-template-columns (str "repeat(" cols ", 1fr)")
     :grid-template-rows    (str "repeat(" rows ", 1fr)")
     :transform (str "translate(" tx ", " ty ")")
     :transform-origin "0 0"}))

(defn grid-component
  "Grid — a 2D grid of workspaces with camera-based navigation.

   Workspaces are a map keyed by [x y] position vectors:
     {[0 0] {:layout LayoutNode}
      [1 0] {:layout LayoutNode}}

   LayoutNode is either:
     {:type \"tile\"  :id string :entity-id string}
     {:type \"split\" :id string :direction \"horizontal\"|\"vertical\"
      :ratio 0-1 :children [LayoutNode LayoutNode]}

   Props (CLJS keywords):
     :workspaces               - {[x y] {:layout LayoutNode}}
     :active-position          - [x y]
     :active-entity            - entity-id string or nil
     :entities                 - map of entity-id → entity map
     :render-entity-tile       - Reagent component fn, receives entity map
     :on-workspaces-change     - fn(workspaces)
     :on-active-position-change - fn([x y])
     :on-active-entity-change  - fn(entity-id)
     :on-entity-close          - fn(entity-id)
     :on-entity-color-change   - fn(entity-id color)
     :logo                     - hiccup for the command center trigger icon"
  [_]
  (util/inject-css!)
  (let [command-center? (r/atom false)
        props-ref (atom nil)
        nav-drag-edge (r/atom nil)
        keydown-handler (fn [e]
                          (when (and (= (.-key e) "Alt") (not (.-repeat e)))
                            (reset! command-center? true))
                          (when (.-altKey e)
                            (let [dir (case (.-key e)
                                        "ArrowLeft"  :left
                                        "ArrowRight" :right
                                        "ArrowUp"    :up
                                        "ArrowDown"  :down
                                        nil)]
                              (when dir
                                (.preventDefault e)
                                (reset! command-center? false)
                                (grid-interactions/handle-grid-nav dir props-ref)))))
        keyup-handler (fn [e]
                        (when (= (.-key e) "Alt")
                          (reset! command-center? false)))]
    (.addEventListener js/document "keydown" keydown-handler)
    (.addEventListener js/document "keyup" keyup-handler)
    (fn [{:keys [workspaces active-position entities render-entity-tile
                 on-active-position-change on-workspaces-change logo] :as props}]
      (reset! props-ref props)
      (let [[cols rows] (grid-dimensions workspaces)
            dragging? @tile/dragging-tile
            vis? (fn [dir] (or dragging? (grid-interactions/can-navigate? dir workspaces active-position)))
            nav-handler (fn [dir] #(grid-interactions/handle-grid-nav dir props-ref))]
        [:div.iris-grid-viewport
         [nav-edge {:css-class "iris-nav-left" :visible? (vis? :left) :on-click (nav-handler :left) :direction :left :props-ref props-ref :nav-drag-edge nav-drag-edge}]
         [nav-edge {:css-class "iris-nav-right" :visible? (vis? :right) :on-click (nav-handler :right) :direction :right :props-ref props-ref :nav-drag-edge nav-drag-edge}]
         [nav-edge {:css-class "iris-nav-top" :visible? (vis? :up) :on-click (nav-handler :up) :direction :up :props-ref props-ref :nav-drag-edge nav-drag-edge}]
         [nav-edge {:css-class "iris-nav-bottom" :visible? (vis? :down) :on-click (nav-handler :down) :direction :down :props-ref props-ref :nav-drag-edge nav-drag-edge}]
         [:div.iris-grid-center
          [:div.iris-grid-canvas
           {:style (camera-style cols rows active-position)}
           (grid-canvas cols rows workspaces active-position props)]]
         [:div.iris-command-center-trigger
          {:on-click (fn [_] (reset! command-center? true))}
          [:svg.iris-command-center-trigger-bg {:viewBox "0 0 48 48" :width "100%" :height "100%"}
           [:path {:d "M0 48 L0 0 A48 48 0 0 1 48 48 Z"
                   :fill "currentColor"}]]
          [:div.iris-command-center-trigger-logo
           (if logo
             logo
             [:svg {:viewBox "0 0 24 24" :width "20" :height "20"}
              [:circle {:cx 12 :cy 12 :r 7
                        :fill "none"
                        :stroke "currentColor"
                        :stroke-width 1.5}]
              [:circle {:cx 12 :cy 12 :r 2.5
                        :fill "currentColor"}]])]]
         [command-center/command-center-overlay cols rows workspaces active-position
          entities render-entity-tile on-active-position-change
          on-workspaces-change command-center?]]))))
