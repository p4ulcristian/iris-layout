(ns iris-layout.components.entity-tile-group
  "Entity tile group — recursive layout tree renderer for the Body.

   Walks the layout tree and renders:
   - :tile nodes as `entity-tile-component` (leaf — single entity)
   - :split nodes as flex containers with two children and a resizer

   Each split provides 'parent context' to its children so tiles can
   detect no-op rearrangements during drag-drop."
  (:require [iris-layout.layout :as layout]
            [iris-layout.components.entity-tile :as entity-tile]
            [iris-layout.components.resizer :as resizer]))

(defn entity-tile-group
  "Recursively render a layout tree node.

   Arguments:
   - node               : layout node (tile or split)
   - on-split           : fn(target-tile-id entity-id direction source-type)
   - on-ratio-change    : fn(split-id new-ratio)
   - active-entity      : entity-id of the focused entity (or nil)
   - entities           : map of entity-id → entity data
   - render-entity-tile : React component fn, receives entity data as props
   - parent-ctx         : (optional) {:direction :sibling-id :child-index}"
  ([node on-split on-ratio-change active-entity entities render-entity-tile]
   (entity-tile-group node on-split on-ratio-change active-entity entities render-entity-tile nil))
  ([node on-split on-ratio-change active-entity entities render-entity-tile parent-ctx]
   (case (layout/node-type node)
     :tile
     [entity-tile/entity-tile-component
      node
      on-split
      (= (:entity-id node) active-entity)
      entities
      render-entity-tile
      parent-ctx]

     :split
     (let [[child1 child2] (:children node)
           direction (:direction node)
           ratio (:ratio node)
           ;; Context tells each child about its sibling for no-op detection
           ctx1 {:direction direction
                 :sibling-id (:id child2)
                 :child-index 0}
           ctx2 {:direction direction
                 :sibling-id (:id child1)
                 :child-index 1}]
       [:div.iris-entity-tile-group
        {:class (name direction)}
        [:div {:style {:flex ratio}}
         [entity-tile-group child1 on-split on-ratio-change active-entity entities render-entity-tile ctx1]]
        [resizer/resizer direction (:id node) on-ratio-change]
        [:div {:style {:flex (- 1 ratio)}}
         [entity-tile-group child2 on-split on-ratio-change active-entity entities render-entity-tile ctx2]]])

     [:div "Unknown node type"])))
