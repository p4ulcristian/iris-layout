(ns iris-layout.util
  (:require [shadow.resource :as rc]))

(defn generate-id []
  (str (random-uuid)))

(defn inject-css! []
  (when-not (js/document.getElementById "iris-layout-styles")
    (let [style (.createElement js/document "style")]
      (set! (.-id style) "iris-layout-styles")
      (set! (.-textContent style) (rc/inline "iris_layout/css/styles.css"))
      (.appendChild (.-head js/document) style))))
