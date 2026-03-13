(ns iris-layout.components.entity-tile
  "Entity tile component — renders a single entity inside the Body layout."
  (:require [reagent.core :as r]
            [react-dom :as react-dom]
            [iris-layout.components.touch-drag :as touch-drag]))

(def preset-colors
  ["#6366f1" "#8b5cf6" "#ec4899" "#f43f5e" "#f97316"
   "#eab308" "#22c55e" "#14b8a6" "#06b6d4" "#3b82f6" "#8b949e"])

(defn color-picker-popover
  "Color swatch popover for changing tile color. Rendered as a portal to escape overflow:hidden."
  [entity-id on-color-change show? anchor-rect active-color]
  (let [close-handler (atom nil)]
    (r/create-class
      {:component-did-mount
       (fn [_]
         (let [handler (fn [_] (reset! show? false))]
           (reset! close-handler handler)
           (js/setTimeout #(.addEventListener js/document "click" handler) 0)))
       :component-will-unmount
       (fn [_]
         (when @close-handler
           (.removeEventListener js/document "click" @close-handler)))
       :reagent-render
       (fn [entity-id on-color-change _show? anchor-rect active-color]
         (let [colors (if (and active-color (not (some #{active-color} preset-colors)))
                        (cons active-color preset-colors)
                        preset-colors)]
           (react-dom/createPortal
             (r/as-element
               [:div.iris-color-picker-popover
                {:style {:top (+ (:bottom anchor-rect) 6)
                         :left (:left anchor-rect)}
                 :on-click (fn [e] (.stopPropagation e))}
                (doall
                 (for [color colors]
                   ^{:key color}
                   [:div {:class (str "iris-color-swatch"
                                      (when (= color active-color) " iris-color-swatch-active"))
                          :style {:background color}
                          :on-click (fn [e]
                                      (.stopPropagation e)
                                      (when on-color-change
                                        (on-color-change entity-id color))
                                      (reset! show? false))}]))])
           js/document.body)))})))

(defn calculate-half
  "Determine which half of the tile the cursor is closest to."
  [e tile-elem]
  (let [rect (.getBoundingClientRect tile-elem)
        cx (+ (.-left rect) (/ (.-width rect) 2))
        cy (+ (.-top rect) (/ (.-height rect) 2))
        dx (- (.-clientX e) cx)
        dy (- (.-clientY e) cy)]
    (if (> (js/Math.abs dx) (js/Math.abs dy))
      (if (neg? dx) :left :right)
      (if (neg? dy) :top :bottom))))

(defn half->direction [half]
  (if (or (= half :left) (= half :right))
    :horizontal
    :vertical))

(def direction-labels
  {:top "Split above" :bottom "Split below"
   :left "Split left" :right "Split right"})

(defn drop-indicator [half visible?]
  (when visible?
    [:div.iris-drop-indicator
     [:div {:class (str "iris-drop-highlight iris-drop-" (name half))}]
     [:div.iris-drop-label-container
      [:span.iris-drop-label (get direction-labels half "Split")]]]))

;; Global atoms
(defonce drag-source-tile (atom nil))
(defonce drag-source-entity (atom nil))
(defonce fullscreen-tile (r/atom nil))

;; --- Extracted handler fns ---

(defn- clear-drop-state! [drag-over closest-edge]
  (reset! drag-over false)
  (reset! closest-edge nil))

(defn- handle-drag-over [e dragging drag-over closest-edge tile-ref]
  (.preventDefault e)
  (if @dragging
    (clear-drop-state! drag-over closest-edge)
    (when-let [el @tile-ref]
      (let [half (calculate-half e el)]
        (reset! closest-edge half)
        (reset! drag-over true)))))

(defn- handle-drop [e node tile-ref split-ref drag-over closest-edge]
  (.preventDefault e)
  (.stopPropagation e)
  (when-let [el @tile-ref]
    (let [half (calculate-half e el)
          direction (half->direction half)
          raw (.getData (.-dataTransfer e) "text/plain")]
      (try
        (let [data (js/JSON.parse raw)]
          (cond
            (and (= (.-source data) "tile") (.-entityId data))
            (when (not= (.-tileId data) (:id node))
              (@split-ref (:id node) (.-entityId data) direction :tile half))

            (= (.-source data) "sidebar")
            (when-let [eid (.-entityId data)]
              (@split-ref (:id node) eid direction :sidebar half))))
        (catch :default _
          (when (and raw (not= raw ""))
            (@split-ref (:id node) raw direction :sidebar))))))
  (clear-drop-state! drag-over closest-edge))

(defn- handle-touch-end [dragging]
  (touch-drag/cancel-touch!)
  (when (touch-drag/dragging?)
    (touch-drag/end-drag!))
  (reset! dragging false))

(defn- handle-drag-start [e node dragging]
  (.setData (.-dataTransfer e) "text/plain"
            (js/JSON.stringify #js {:tileId (:id node)
                                     :entityId (:entity-id node)
                                     :source "tile"}))
  (set! (.-effectAllowed (.-dataTransfer e)) "all")
  (reset! drag-source-tile (:id node))
  (reset! drag-source-entity (:entity-id node))
  (reset! dragging true))

(defn- handle-drag-end [dragging]
  (reset! dragging false)
  (reset! drag-source-tile nil)
  (reset! drag-source-entity nil))

;; --- Main component ---

(defn entity-tile-component
  [node on-split on-close focused? entities render-entity-tile _parent-ctx on-active-entity-change on-entity-color-change]
  (let [drag-over (r/atom false)
        closest-edge (r/atom nil)
        dragging (r/atom false)
        color-picker-open? (r/atom false)
        color-picker-rect (r/atom nil)
        split-ref (atom on-split)
        close-ref (atom on-close)
        active-entity-change-ref (atom on-active-entity-change)
        color-change-ref (atom on-entity-color-change)
        tile-ref (atom nil)
        touch-watch-key (str "tile-" (:id node))
        drop-watch-key (str "tile-drop-" (:id node))
        _ (add-watch touch-drag/hover-target touch-watch-key
            (fn [_ _ _ new-target]
              (if (and new-target (= (:tile-id new-target) (:id node)))
                (let [source-id (:tile-id @touch-drag/touch-state)]
                  (if (= source-id (:id node))
                    (clear-drop-state! drag-over closest-edge)
                    (do (reset! closest-edge (:half new-target))
                        (reset! drag-over true))))
                (when @drag-over
                  (clear-drop-state! drag-over closest-edge)))))
        _ (add-watch touch-drag/drop-result drop-watch-key
            (fn [_ _ _ drop-info]
              (when (and drop-info (= (:target-tile-id drop-info) (:id node)))
                (let [{:keys [source-tile-id source-entity-id half]} drop-info
                      direction (half->direction half)]
                  (when (not= source-tile-id (:id node))
                    (@split-ref (:id node) source-entity-id direction :tile half)))
                (reset! touch-drag/drop-result nil)
                (clear-drop-state! drag-over closest-edge))))]
    (r/create-class
      {:component-will-unmount
       (fn [_]
         (remove-watch touch-drag/hover-target touch-watch-key)
         (remove-watch touch-drag/drop-result drop-watch-key))

       :reagent-render
       (fn [node on-split on-close focused? entities render-entity-tile _parent-ctx on-active-entity-change on-entity-color-change]
         (reset! split-ref on-split)
         (reset! close-ref on-close)
         (reset! active-entity-change-ref on-active-entity-change)
         (reset! color-change-ref on-entity-color-change)
         (let [entity (get entities (:entity-id node))
               entity-name (or (:name entity) (:entity-id node))
               tile-color (or (:color entity) "#6366f1")]
           [:div
            {:ref #(reset! tile-ref %)
             :data-tile-id (:id node)
             :class (str "iris-entity-tile"
                         (when focused? " iris-entity-tile-focused")
                         (when (not focused?) " iris-entity-tile-unfocused")
                         (when @drag-over " iris-drag-over")
                         (when @dragging " iris-dragging"))
             :style (cond-> {:flex 1}
                            (:color entity) (assoc "--iris-tile-color" (:color entity)))
             :on-mouse-enter (fn [_]
                                (when (and (not focused?) @active-entity-change-ref)
                                  (@active-entity-change-ref (:entity-id node))))
             :on-drag-over #(handle-drag-over % dragging drag-over closest-edge tile-ref)
             :on-drag-enter (fn [e] (.preventDefault e))
             :on-drag-leave (fn [e]
                              (when-not (.contains (.-currentTarget e) (.-relatedTarget e))
                                (clear-drop-state! drag-over closest-edge)))
             :on-drop #(handle-drop % node tile-ref split-ref drag-over closest-edge)
 }

            ;; Header
            [:div.iris-entity-tile-header
             {:draggable true
              :on-double-click (fn [_]
                                 (if (= @fullscreen-tile (:id node))
                                   (reset! fullscreen-tile nil)
                                   (reset! fullscreen-tile (:id node))))
              :on-drag-start #(handle-drag-start % node dragging)
              :on-drag-end (fn [_] (handle-drag-end dragging))
              :on-touch-start (fn [e]
                                (touch-drag/start-touch!
                                  (:id node) (:entity-id node) e))
              :on-touch-end (fn [_e] (handle-touch-end dragging))}
             [:div {:style {:flex-shrink 0}}
              [:div.iris-entity-tile-header-dot
               {:style {:background tile-color}
                :on-click (fn [e]
                            (.stopPropagation e)
                            (let [rect (.getBoundingClientRect (.-currentTarget e))]
                              (reset! color-picker-rect
                                      {:top (.-top rect) :left (.-left rect)
                                       :bottom (.-bottom rect) :width (.-width rect)}))
                            (swap! color-picker-open? not))}]
              (when @color-picker-open?
                [color-picker-popover (:entity-id node) @color-change-ref color-picker-open? @color-picker-rect tile-color])]
             [:span.iris-entity-tile-header-name entity-name]
             [:button.iris-entity-tile-header-close
              {:on-click (fn [e]
                           (.stopPropagation e)
                           (when @close-ref (@close-ref (:entity-id node))))}
              "\u00d7"]]

            ;; Content
            [:div.iris-entity-tile-content
             (when (and entity render-entity-tile)
               [:> render-entity-tile entity])]

            ;; Drop indicator
            [drop-indicator @closest-edge @drag-over]]))})))
