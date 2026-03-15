(ns iris-layout.util)

(defn generate-id []
  (str (random-uuid)))

(defn workspace-at
  "Find the workspace entry [id ws] at position [x y]. Returns nil if not found."
  [workspaces x y]
  (some (fn [[id ws]] (when (and (= (:x ws) x) (= (:y ws) y)) [id ws]))
        workspaces))
