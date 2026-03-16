(ns iris-layout.util
  (:require-macros [iris-layout.css :refer [inline-css]]))

(defn generate-id []
  (str (random-uuid)))

(defn inject-css! []
  (when-not (js/document.getElementById "iris-layout-styles")
    (let [style (.createElement js/document "style")]
      (set! (.-id style) "iris-layout-styles")
      (set! (.-textContent style) (inline-css))
      (.appendChild (.-head js/document) style))))
