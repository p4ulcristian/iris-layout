(ns iris-layout.core
  "iris-layout — a 2D tiling workspace grid for Reagent."
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.util :as util]
            [iris-layout.pane.pane :as pane]
            [iris-layout.pane.tile :as tile]
            [iris-layout.drag.touch :as touch-drag]
            [iris-layout.workspace.command-center :as command-center])
  (:require-macros [iris-layout.css :refer [inline-css]]))

(r/set-default-compiler! (r/create-compiler {:function-components true}))

(defn- inject-css! []
  (when-not (js/document.getElementById "iris-layout-styles")
    (let [style (.createElement js/document "style")]
      (set! (.-id style) "iris-layout-styles")
      (set! (.-textContent style) (inline-css))
      (.appendChild (.-head js/document) style))))


;; ============================================================
;; Body stage — single workspace layout renderer
;; ============================================================

(defn- handle-split
  "Insert a new tile into the layout by splitting an existing one."
  [layout on-layout-change tile-id entity-id split-direction half source-tile-id]
  (let [before? (or (= half :left) (= half :top))
        base-layout (or (layout/remove-tile-from-layout layout source-tile-id) layout)
        target (layout/find-tile base-layout tile-id)
        new-layout (when target
                     (layout/split-tile base-layout tile-id split-direction
                                        entity-id (util/generate-id) (util/generate-id) before?))]
    (when (and new-layout on-layout-change)
      (on-layout-change new-layout))))

(defn- handle-close
  "Remove a tile from the layout and notify the consumer."
  [layout on-layout-change on-entity-close tile-id]
  (let [tile (layout/find-tile layout tile-id)
        new-layout (layout/remove-tile-from-layout layout tile-id)]
    (when on-layout-change (on-layout-change new-layout))
    (when on-entity-close (on-entity-close (:entity-id tile)))))

(defn- handle-ratio
  "Update the split ratio of a split node."
  [layout on-layout-change split-id new-ratio]
  (when on-layout-change
    (on-layout-change (layout/update-split-ratio layout split-id new-ratio))))

(defn- body-stage-component
  [{:keys [layout entities render-entity-tile active-entity
           on-layout-change on-active-entity-change on-entity-color-change on-entity-close]}]
  [:div.iris-body-stage
   [pane/pane
    layout
    (partial handle-split layout on-layout-change)
    (partial handle-close layout on-layout-change on-entity-close)
    (partial handle-ratio layout on-layout-change)
    active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]])


;; ============================================================
;; Grid — 2D workspace grid with camera-based navigation
;; ============================================================

(defn- direction->delta [dir]
  (case dir
    :left  [-1 0]
    :right [1 0]
    :up    [0 -1]
    :down  [0 1]))

(defn- grid-dimensions
  "Compute grid bounds from workspace [x y] keys. Returns [cols rows]."
  [workspaces]
  (if (empty? workspaces)
    [1 1]
    [(inc (apply max (map first (keys workspaces))))
     (inc (apply max (map second (keys workspaces))))]))

(defn- can-navigate?
  [dir workspaces active-position]
  (let [[dx dy] (direction->delta dir)
        [x y] active-position
        new-x (+ x dx)
        new-y (+ y dy)
        active-ws      (get workspaces [x y])
        current-empty? (nil? (:layout active-ws))
        target-ws      (get workspaces [new-x new-y])
        target-has-layout? (boolean (and target-ws (:layout target-ws)))]
    (and (>= new-x 0) (>= new-y 0)
         (not (and current-empty? (not target-has-layout?))))))

(defn- handle-grid-nav
  [dir props-ref]
  (let [{:keys [workspaces active-position
                on-workspaces-change on-active-position-change]} @props-ref]
    (when (can-navigate? dir workspaces active-position)
      (let [[dx dy] (direction->delta dir)
            [x y] active-position
            new-pos [(+ x dx) (+ y dy)]]
        (when (and (not (get workspaces new-pos)) on-workspaces-change)
          (on-workspaces-change (assoc workspaces new-pos {:layout nil})))
        (when on-active-position-change
          (on-active-position-change new-pos))))))

(defn- nav-edge-hiccup
  [css-class visible? on-click direction props-ref nav-drag-edge]
  [:div {:class (str "iris-nav-edge " css-class
                     (when-not visible? " iris-nav-hidden")
                     (when (= @nav-drag-edge direction) " iris-nav-drag-over"))
         :on-click on-click
         :on-drag-over (fn [e] (.preventDefault e))
         :on-drag-enter (fn [e]
                          (.preventDefault e)
                          (reset! nav-drag-edge direction)
                          (handle-grid-nav direction props-ref))
         :on-drag-leave (fn [e]
                          (when-not (.contains (.-currentTarget e) (.-relatedTarget e))
                            (reset! nav-drag-edge nil)))
         :on-drop (fn [e]
                    (.preventDefault e)
                    (.stopPropagation e)
                    (reset! nav-drag-edge nil))}
   [:div.iris-nav-semicircle]])

(defn- update-workspaces-with-cleanup
  "Update a workspace's layout and remove dragged tile from all other workspaces."
  [workspaces target-pos new-layout]
  (let [dragged-tile @tile/drag-source-tile]
    (if dragged-tile
      (reduce-kv
        (fn [acc pos ws-data]
          (if (= pos target-pos)
            (assoc acc pos (assoc ws-data :layout new-layout))
            (assoc acc pos (assoc ws-data :layout
                             (layout/remove-tile-from-layout (:layout ws-data) dragged-tile)))))
        {} workspaces)
      (update workspaces target-pos assoc :layout new-layout))))

(defn- handle-empty-workspace-drop
  [e x y workspaces on-workspaces-change]
  (.preventDefault e)
  (let [raw (.getData (.-dataTransfer e) "text/plain")]
    (try
      (let [data (js/JSON.parse raw)
            entity-id (.-entityId data)]
        (when entity-id
          (let [pos [x y]
                workspaces (if (get workspaces pos) workspaces
                               (assoc workspaces pos {:layout nil}))
                new-layout {:type :tile :id (util/generate-id) :entity-id entity-id}
                updated (update-workspaces-with-cleanup workspaces pos new-layout)]
            (on-workspaces-change updated))))
      (catch :default _ nil))))

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
                      (handle-empty-workspace-drop e x y workspaces on-workspaces-change)))}])]))

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
  (inject-css!)
  (let [command-center? (r/atom false)
        props-ref (atom nil)
        nav-drag-edge (r/atom nil)
        touch-nav-happened? (atom false)
        handle-nav (fn [dir] (handle-grid-nav dir props-ref))
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
                                (handle-nav dir)))))
        keyup-handler (fn [e]
                        (when (= (.-key e) "Alt")
                          (reset! command-center? false)))]
    (.addEventListener js/document "keydown" keydown-handler)
    (.addEventListener js/document "keyup" keyup-handler)
    (add-watch touch-drag/nav-edge-target ::grid-nav-highlight
      (fn [_ _ old-dir new-dir]
        (reset! nav-drag-edge new-dir)
        (when (and new-dir (not= old-dir new-dir))
          (reset! touch-nav-happened? true)
          (handle-grid-nav new-dir props-ref))))
    (add-watch touch-drag/drop-result ::grid-touch-drop
      (fn [_ _ _ drop-info]
        (when (and drop-info
                   (not (:target-tile-id drop-info))
                   @touch-nav-happened?)
          (let [{:keys [source-tile-id source-entity-id]} drop-info
                {:keys [workspaces active-position on-workspaces-change]} @props-ref
                active-pos active-position
                active-ws  (get workspaces active-pos)]
            (when (and source-entity-id on-workspaces-change)
              (let [new-tile {:type :tile :id (util/generate-id) :entity-id source-entity-id}
                    existing-layout (:layout active-ws)
                    new-layout (if existing-layout
                                 {:type :split :id (util/generate-id)
                                  :direction "horizontal" :ratio 0.5
                                  :children [existing-layout new-tile]}
                                 new-tile)
                    updated (reduce-kv
                              (fn [acc pos ws-data]
                                (if (= pos active-pos)
                                  (assoc acc pos (assoc ws-data :layout new-layout))
                                  (assoc acc pos (assoc ws-data :layout
                                                   (layout/remove-tile-from-layout (:layout ws-data) source-tile-id)))))
                              {} workspaces)]
                (on-workspaces-change updated))))
          (reset! touch-drag/drop-result nil))
        (reset! touch-nav-happened? false)))
    (fn [{:keys [workspaces active-position entities render-entity-tile
                 on-active-position-change on-workspaces-change logo] :as props}]
      (reset! props-ref props)
      (let [[cols rows] (grid-dimensions workspaces)
            dragging? (or @tile/drag-source-tile @touch-drag/touch-state)
            vis? (fn [dir] (or dragging? (can-navigate? dir workspaces active-position)))]
        [:div.iris-grid-viewport
         (nav-edge-hiccup "iris-nav-left" (vis? :left) #(handle-nav :left) :left props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-right" (vis? :right) #(handle-nav :right) :right props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-top" (vis? :up) #(handle-nav :up) :up props-ref nav-drag-edge)
         (nav-edge-hiccup "iris-nav-bottom" (vis? :down) #(handle-nav :down) :down props-ref nav-drag-edge)
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
