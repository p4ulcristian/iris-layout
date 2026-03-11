(ns iris-demo.components.surface
  (:require [iris-demo.components.tile :as tile]
            [iris-demo.components.resizer :as resizer]))

(declare surface)

(defn surface
  "Recursive layout renderer - renders either a tile or a split with children"
  [node on-split on-ratio-change focused-tile entities]
  (case (:type node)
    :tile
    [tile/tile-component
     node
     on-split
     (= (:id node) focused-tile)
     entities]

    :split
    (let [[child1 child2] (:children node)
          direction (:direction node)
          ratio (:ratio node)]
      [:div.split-container
       {:class (name direction)}
       [:div {:style {:flex ratio}}
        [surface child1 on-split on-ratio-change focused-tile entities]]
       [resizer/resizer direction (:id node) on-ratio-change]
       [:div {:style {:flex (- 1 ratio)}}
        [surface child2 on-split on-ratio-change focused-tile entities]]])

    ;; Fallback
    [:div "Unknown node type"]))