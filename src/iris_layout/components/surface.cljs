(ns iris-layout.components.surface
  (:require [iris-layout.layout :as layout]
            [iris-layout.components.tile :as tile]
            [iris-layout.components.resizer :as resizer]))

(defn surface
  "Recursive layout renderer - renders either a tile or a split with children.
   parent-ctx is {:direction :horizontal/:vertical :sibling-id str :child-index 0/1}"
  ([node on-split on-ratio-change focused-tile entities render-entity]
   (surface node on-split on-ratio-change focused-tile entities render-entity nil))
  ([node on-split on-ratio-change focused-tile entities render-entity parent-ctx]
   (case (layout/node-type node)
     :tile
     [tile/tile-component
      node
      on-split
      (= (:id node) focused-tile)
      entities
      render-entity
      parent-ctx]

     :split
     (let [[child1 child2] (:children node)
           direction (:direction node)
           ratio (:ratio node)
           ;; Build context for each child — tells them about their sibling
           ctx1 {:direction direction
                 :sibling-id (:id child2)
                 :child-index 0}
           ctx2 {:direction direction
                 :sibling-id (:id child1)
                 :child-index 1}]
       [:div.iris-split-container
        {:class (name direction)}
        [:div {:style {:flex ratio}}
         [surface child1 on-split on-ratio-change focused-tile entities render-entity ctx1]]
        [resizer/resizer direction (:id node) on-ratio-change]
        [:div {:style {:flex (- 1 ratio)}}
         [surface child2 on-split on-ratio-change focused-tile entities render-entity ctx2]]])

     [:div "Unknown node type"])))
