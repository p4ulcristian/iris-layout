(ns iris-layout.components.touch-drag
  "Touch drag state — shared across all tiles for mobile drag-drop.

   On touch devices, HTML5 Drag and Drop doesn't work. This module
   provides a global touch-drag state that tiles subscribe to.

   Flow:
   1. Header touchstart → records start position and source info
   2. Global touchmove → after small movement, activates drag with ghost
   3. Touchend → performs drop on target tile, cleans up"
  (:require [reagent.core :as r]))

;; Movement threshold in px — drag activates after finger moves this far
(def move-threshold 8)

;; Global touch drag state
(defonce touch-state (r/atom nil))
;; Shape: {:tile-id str :entity-id str :ghost-el DOM :active? bool}

(defonce hover-target (r/atom nil))
;; Shape: {:tile-el DOM :tile-id str :half keyword} or nil

;; Nav edge target — tracks when touch hovers over a nav edge during drag
(defonce nav-edge-target (r/atom nil))
;; Shape: :left | :right | :up | :down | nil

;; Drop result — set when drag ends, watched by target tiles to execute the split
(defonce drop-result (r/atom nil))
;; Shape: {:source-tile-id str :source-entity-id str :target-tile-id str :half keyword} or nil

;; Touch start state (not reactive, internal only)
(defonce ^:private touch-start (atom nil))
;; Shape: {:start-x num :start-y num :tile-id str :entity-id str}

(defn dragging? []
  (some? @touch-state))

(defn touch-started? []
  (some? @touch-start))

(defn- create-ghost! [x y]
  (let [ghost (js/document.createElement "div")]
    (set! (.-className ghost) "iris-touch-drag-ghost")
    (set! (.-cssText (.-style ghost))
          (str "position:fixed;z-index:10000;pointer-events:none;"
               "background:rgba(99,102,241,0.8);color:#fff;padding:6px 14px;"
               "border-radius:8px;font-size:12px;font-weight:500;"
               "box-shadow:0 4px 20px rgba(0,0,0,0.4);"
               "left:" (- x 40) "px;"
               "top:" (- y 20) "px;"))
    (.appendChild js/document.body ghost)
    ghost))

(defn start-touch!
  "Record touch start on a header. Drag activates after movement threshold."
  [tile-id entity-id touch-event]
  (let [touch (aget (.-changedTouches touch-event) 0)
        x (.-clientX touch)
        y (.-clientY touch)]
    (reset! touch-start {:start-x x
                         :start-y y
                         :tile-id tile-id
                         :entity-id entity-id})))

(defn cancel-touch!
  "Cancel a touch start without activating drag."
  []
  (reset! touch-start nil))

(defn- activate-drag!
  "Activate drag — create ghost and set drag state."
  [ts x y]
  (let [ghost (create-ghost! x y)]
    (reset! touch-state {:tile-id (:tile-id ts)
                         :entity-id (:entity-id ts)
                         :ghost-el ghost
                         :active? true})
    (reset! touch-start nil)
    ;; Clear any text selection
    (when-let [sel (js/window.getSelection)]
      (.removeAllRanges sel))
    ;; Vibrate for haptic feedback if available
    (when (.-vibrate js/navigator)
      (.vibrate js/navigator 30))))

(defn- find-tile-element
  "Find the closest .iris-entity-tile ancestor from the element at point."
  [x y]
  (when-let [el (js/document.elementFromPoint x y)]
    (.closest el ".iris-entity-tile")))

(defn- calculate-half-xy
  "Determine which half of an element a point is closest to."
  [x y el]
  (let [rect (.getBoundingClientRect el)
        cx (+ (.-left rect) (/ (.-width rect) 2))
        cy (+ (.-top rect) (/ (.-height rect) 2))
        dx (- x cx)
        dy (- y cy)]
    (if (> (js/Math.abs dx) (js/Math.abs dy))
      (if (neg? dx) :left :right)
      (if (neg? dy) :top :bottom))))

(defn move-drag!
  "Update ghost position and find tile under finger."
  [touch-event]
  (when-let [state @touch-state]
    (let [touch (aget (.-changedTouches touch-event) 0)
          x (.-clientX touch)
          y (.-clientY touch)
          ghost (:ghost-el state)]
      ;; Move ghost
      (when ghost
        (set! (.-left (.-style ghost)) (str (- x 40) "px"))
        (set! (.-top (.-style ghost)) (str (- y 20) "px")))
      ;; Hide ghost briefly to find element underneath
      (when ghost (set! (.-display (.-style ghost)) "none"))
      (let [el-under (js/document.elementFromPoint x y)
            nav-el (when el-under (.closest el-under ".iris-nav-edge"))
            tile-el (find-tile-element x y)]
        (when ghost (set! (.-display (.-style ghost)) ""))
        (if nav-el
          (let [dir (cond
                      (.contains (.-classList nav-el) "iris-nav-left") :left
                      (.contains (.-classList nav-el) "iris-nav-right") :right
                      (.contains (.-classList nav-el) "iris-nav-top") :up
                      (.contains (.-classList nav-el) "iris-nav-bottom") :down
                      :else nil)]
            (reset! nav-edge-target dir)
            (reset! hover-target nil))
          (do
            (reset! nav-edge-target nil)
            (if tile-el
              (let [half (calculate-half-xy x y tile-el)
                    tile-id (.getAttribute tile-el "data-tile-id")]
                (reset! hover-target {:tile-el tile-el :tile-id tile-id :half half}))
              (reset! hover-target nil))))))))

(defn end-drag!
  "End touch drag — publish drop result for target tile or workspace to handle, then clean up."
  []
  (let [state @touch-state
        target @hover-target]
    ;; Publish drop result — with or without a target tile
    (when state
      (reset! drop-result (cond-> {:source-tile-id (:tile-id state)
                                   :source-entity-id (:entity-id state)}
                            target (assoc :target-tile-id (:tile-id target)
                                          :half (:half target)))))
    ;; Clean up ghost
    (when-let [ghost (:ghost-el state)]
      (.remove ghost))
    (reset! touch-state nil)
    (reset! hover-target nil)
    (reset! nav-edge-target nil)))

;; Global touch listeners (attached once)
(defonce _touch-listeners
  (do
    (.addEventListener js/document "touchmove"
      (fn [e]
        (cond
          ;; Active drag — move ghost and find target
          (dragging?)
          (do (.preventDefault e)
              (move-drag! e))
          ;; Touch started on header — check if moved enough to activate drag
          (touch-started?)
          (let [ts @touch-start
                touch (aget (.-changedTouches e) 0)
                x (.-clientX touch)
                y (.-clientY touch)
                dx (- x (:start-x ts))
                dy (- y (:start-y ts))
                dist (js/Math.sqrt (+ (* dx dx) (* dy dy)))]
            (when (> dist move-threshold)
              (.preventDefault e)
              (activate-drag! ts x y)))))
      #js {:passive false})
    (.addEventListener js/document "touchend"
      (fn [_e]
        ;; Cancel touch start on lift (was a tap, not drag)
        (cancel-touch!)
        ;; Safety cleanup for active drag (individual tiles handle the drop)
        (when (dragging?)
          (js/setTimeout #(when (dragging?) (end-drag!)) 100))
        ;; Remove any leftover ghost elements
        (js/setTimeout
          (fn []
            (doseq [ghost (array-seq (.querySelectorAll js/document ".iris-touch-drag-ghost"))]
              (when-not (dragging?)
                (.remove ghost))))
          300))
      #js {:passive true})
    ;; Prevent context menu during drag
    (.addEventListener js/document "contextmenu"
      (fn [e]
        (when (or (touch-started?) (dragging?))
          (.preventDefault e)))
      #js {:passive false})
    (.addEventListener js/document "touchcancel"
      (fn [_e]
        (cancel-touch!)
        (when (dragging?) (end-drag!))
        ;; Remove any orphaned ghost elements
        (js/setTimeout
          (fn []
            (doseq [ghost (array-seq (.querySelectorAll js/document ".iris-touch-drag-ghost"))]
              (.remove ghost)))
          100))
      #js {:passive true})
    true))
