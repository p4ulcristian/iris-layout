(ns iris-layout.interactions.grid
  (:require [iris-layout.interactions.tile :as tile-interactions]
            [iris-layout.util :as util]))

(defn direction->delta [dir]
  (case dir
    :left  [-1 0]
    :right [1 0]
    :up    [0 -1]
    :down  [0 1]))

(defn can-navigate?
  [dir _workspaces active-position]
  (let [[dx dy] (direction->delta dir)
        [x y] active-position
        new-x (+ x dx)
        new-y (+ y dy)]
    (and (>= new-x 0) (< new-x 3)
         (>= new-y 0) (< new-y 3))))

(defn handle-grid-nav
  [dir props-ref]
  (let [{:keys [workspaces active-position on-active-position-change]} @props-ref]
    (when (can-navigate? dir workspaces active-position)
      (let [[dx dy] (direction->delta dir)
            [x y] active-position
            new-pos [(+ x dx) (+ y dy)]]
        (when on-active-position-change
          (on-active-position-change new-pos))))))

(defn navigate-to-workspace
  "Navigate to a workspace cell on click — used by the command center mini grid."
  [pos on-active-position-change]
  (when on-active-position-change
    (on-active-position-change pos)))

(defn handle-empty-workspace-drop
  "Drop a tile onto an empty workspace cell."
  [x y workspaces on-workspaces-change tile-id entity-id]
  (let [pos        [x y]
        ws         (or (get workspaces pos) {:layout nil})
        new-layout {:type :tile :id (util/generate-id) :entity-id entity-id}
        updated    (tile-interactions/update-workspaces-with-cleanup
                     (if (get workspaces pos) workspaces (assoc workspaces pos ws))
                     pos new-layout tile-id)]
    (on-workspaces-change updated)))
