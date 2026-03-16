(ns iris-layout.interactions.tile
  (:require [iris-layout.layout :as layout]
            [iris-layout.util :as util]))

(defn handle-split
  "Insert a new tile into the layout by splitting an existing one."
  [layout on-layout-change tile-id entity-id split-direction half source-tile-id]
  (let [before? (or (= half :left) (= half :top))
        base-layout (or (layout/remove-tile-from-layout layout source-tile-id) layout)
        target (layout/find-tile base-layout tile-id)
        new-layout (when target
                     (layout/split-tile base-layout tile-id split-direction
                                        entity-id (util/generate-id) (util/generate-id) before?))]
    (when (and new-layout on-layout-change)
      (on-layout-change new-layout))))

(defn handle-close
  "Remove a tile from the layout and notify the consumer."
  [layout on-layout-change on-entity-close tile-id]
  (let [tile (layout/find-tile layout tile-id)
        new-layout (layout/remove-tile-from-layout layout tile-id)]
    (when on-layout-change (on-layout-change new-layout))
    (when on-entity-close (on-entity-close (:entity-id tile)))))

(defn handle-ratio
  "Update the split ratio of a split node."
  [layout on-layout-change split-id new-ratio]
  (when on-layout-change
    (on-layout-change (layout/update-split-ratio layout split-id new-ratio))))

(defn update-workspaces-with-cleanup
  "Update a workspace's layout and remove dragged tile from all other workspaces."
  [workspaces target-pos new-layout drag-source-tile]
  (if drag-source-tile
    (reduce-kv
      (fn [acc pos ws-data]
        (if (= pos target-pos)
          (assoc acc pos (assoc ws-data :layout new-layout))
          (assoc acc pos (assoc ws-data :layout
                           (layout/remove-tile-from-layout (:layout ws-data) drag-source-tile)))))
      {} workspaces)
    (update workspaces target-pos assoc :layout new-layout)))
