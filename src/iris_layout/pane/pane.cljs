(ns iris-layout.pane.pane
  "Pane — recursive layout tree renderer."
  (:require [iris-layout.layout :as layout]
            [iris-layout.pane.tile :as tile]
            [iris-layout.pane.resizer :as resizer]))

(declare pane)

(defn tile-pane
  [node on-close active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]
  [tile/entity-tile-component
   {:node node
    :on-close on-close
    :focused? (= (:entity-id node) active-entity)
    :entities entities
    :render-entity-tile render-entity-tile
    :on-active-entity-change on-active-entity-change
    :on-entity-color-change on-entity-color-change}])

(defn split-pane
  [node on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]
  (let [[child1 child2] (:children node)
        direction (:direction node)
        ratio (:ratio node)]
    [:div.iris-entity-tile-group
     {:class direction}
     [:div {:style {:flex ratio}}
      [pane child1 on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]]
     [resizer/resizer direction (:id node) on-ratio-change]
     [:div {:style {:flex (- 1 ratio)}}
      [pane child2 on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]]]))

(defn pane
  "Recursively render a layout tree node as either a tile or a split pane."
  [node on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]
  (case (layout/node-type node)
    :tile  [tile-pane node on-close active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]
    :split [split-pane node on-close on-ratio-change active-entity entities render-entity-tile on-active-entity-change on-entity-color-change]
    [:div "Unknown node type"]))
