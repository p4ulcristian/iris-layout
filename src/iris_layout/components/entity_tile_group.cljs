(ns iris-layout.components.entity-tile-group
  "Entity tile group — recursive layout tree renderer for the Body."
  (:require [iris-layout.layout :as layout]
            [iris-layout.components.entity-tile :as entity-tile]
            [iris-layout.components.resizer :as resizer]))

(defn entity-tile-group
  "Recursively render a layout tree node."
  [node on-split on-close on-ratio-change active-entity entities render-entity-tile]
  (case (layout/node-type node)
    :tile
    [entity-tile/entity-tile-component
     node on-split on-close
     (= (:entity-id node) active-entity)
     entities render-entity-tile nil]

    :split
    (let [[child1 child2] (:children node)
          direction (:direction node)
          ratio (:ratio node)]
      [:div.iris-entity-tile-group
       {:class (name direction)}
       [:div {:style {:flex ratio}}
        [entity-tile-group child1 on-split on-close on-ratio-change active-entity entities render-entity-tile]]
       [resizer/resizer direction (:id node) on-ratio-change]
       [:div {:style {:flex (- 1 ratio)}}
        [entity-tile-group child2 on-split on-close on-ratio-change active-entity entities render-entity-tile]]])

    [:div "Unknown node type"]))
