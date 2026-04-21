(ns iris-layout.mobile.gestures
  (:require [reagent.hooks :as rh]))

(defn use-swipe
  "Returns touch handlers that call on-swipe with :left :right :up :down."
  [on-swipe {:keys [threshold] :or {threshold 50}}]
  (let [touch-start (rh/use-ref nil)]
    {:on-touch-start
     (fn [e]
       (let [t (aget (.-touches e) 0)]
         (set! (.-current touch-start) #js {:x (.-clientX t) :y (.-clientY t)})))
     :on-touch-end
     (fn [e]
       (when-let [start (.-current touch-start)]
         (let [t   (aget (.-changedTouches e) 0)
               dx  (- (.-clientX t) (.-x start))
               dy  (- (.-clientY t) (.-y start))
               adx (js/Math.abs dx)
               ady (js/Math.abs dy)]
           (set! (.-current touch-start) nil)
           (when (or (> adx threshold) (> ady threshold))
             (on-swipe (if (> adx ady)
                         (if (neg? dx) :left :right)
                         (if (neg? dy) :up :down)))))))}))
