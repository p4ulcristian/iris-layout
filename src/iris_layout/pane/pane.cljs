(ns iris-layout.pane.pane
  "Pane — recursive layout tree renderer."
  (:require [iris-layout.layout :as layout]
            [iris-layout.pane.tile :as tile]
            [iris-layout.pane.resizer :as resizer]))

(defn pane
  "Recursively render a layout tree node as either a tile or a split pane."
  [node on-split on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]
  (case (layout/node-type node)
    :tile
    [tile/entity-tile-component
     node on-split on-close
     (= (:entity-id node) active-entity)
     entities render-entity-tile nil on-active-entity-change on-entity-color-change]

    :split
    (let [[child1 child2] (:children node)
          direction (:direction node)
          ratio (:ratio node)]
      [:div.iris-entity-tile-group
       {:class direction}
       [:div {:style {:flex ratio}}
        [pane child1 on-split on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]]
       [resizer/resizer direction (:id node) on-ratio-change]
       [:div {:style {:flex (- 1 ratio)}}
        [pane child2 on-split on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]]])

    [:div "Unknown node type"]))
