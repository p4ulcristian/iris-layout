(ns iris-layout.components.surface
  (:require [iris-layout.layout :as layout]
            [iris-layout.components.tile :as tile]
            [iris-layout.components.resizer :as resizer]))

(defn surface
  "Recursive layout renderer - renders either a tile or a split with children"
  [node on-split on-ratio-change focused-tile entities render-entity]
  (case (layout/node-type node)
    :tile
    [tile/tile-component
     node
     on-split
     (= (:id node) focused-tile)
     entities
     render-entity]

    :split
    (let [[child1 child2] (:children node)
          direction (:direction node)
          ratio (:ratio node)]
      [:div.iris-split-container
       {:class (name direction)}
       [:div {:style {:flex ratio}}
        [surface child1 on-split on-ratio-change focused-tile entities render-entity]]
       [resizer/resizer direction (:id node) on-ratio-change]
       [:div {:style {:flex (- 1 ratio)}}
        [surface child2 on-split on-ratio-change focused-tile entities render-entity]]])

    [:div "Unknown node type"]))
