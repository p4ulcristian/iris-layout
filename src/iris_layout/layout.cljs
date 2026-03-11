(ns iris-layout.layout)

(defn node-type
  "Get node type as keyword (handles both keyword and string values)"
  [node]
  (keyword (:type node)))

(defn create-tile
  "Create a new tile node"
  [id entity-id]
  {:type :tile
   :id id
   :entity-id entity-id})

(defn create-split
  "Create a new split node with direction, children, and ratio"
  [id direction child1 child2 ratio]
  {:type :split
   :id id
   :direction direction
   :ratio (min 0.9 (max 0.1 ratio))
   :children [child1 child2]})

(defn find-tile
  "Walk tree to find tile by ID"
  [tree tile-id]
  (cond
    (nil? tree) nil
    (= (node-type tree) :tile)
    (when (= (:id tree) tile-id) tree)

    (= (node-type tree) :split)
    (or (find-tile (first (:children tree)) tile-id)
        (find-tile (second (:children tree)) tile-id))))

(defn replace-node
  "Replace a node in the tree by ID with a new node"
  [tree node-id new-node]
  (cond
    (nil? tree) nil

    (= (:id tree) node-id)
    new-node

    (= (node-type tree) :split)
    (assoc tree :children
           [(replace-node (first (:children tree)) node-id new-node)
            (replace-node (second (:children tree)) node-id new-node)])

    :else tree))

(defn split-tile
  "Insert a split at tile position with new entity.
   When before? is true, new tile goes first (left/top)."
  [tree tile-id direction new-entity-id new-tile-id split-id & [before?]]
  (let [existing-tile (find-tile tree tile-id)]
    (when existing-tile
      (let [new-tile (create-tile new-tile-id new-entity-id)
            [child1 child2] (if before?
                              [new-tile existing-tile]
                              [existing-tile new-tile])
            new-split (create-split split-id direction child1 child2 0.5)]
        (replace-node tree tile-id new-split)))))

(defn get-parent-split
  "Find the parent split of a given node"
  [tree node-id]
  (letfn [(find-parent [node parent]
            (cond
              (nil? node) nil

              (= (:id node) node-id) parent

              (= (node-type node) :split)
              (or (find-parent (first (:children node)) node)
                  (find-parent (second (:children node)) node))))]
    (find-parent tree nil)))

(defn merge-tile
  "Remove a tile and collapse its parent split"
  [tree tile-id]
  (let [parent (get-parent-split tree tile-id)]
    (if parent
      (let [[child1 child2] (:children parent)
            sibling (if (= (:id child1) tile-id) child2 child1)]
        (replace-node tree (:id parent) sibling))
      tree)))

(defn find-tile-by-entity
  "Find the tile node that contains a given entity-id"
  [tree entity-id]
  (cond
    (nil? tree) nil
    (= (node-type tree) :tile)
    (when (= (:entity-id tree) entity-id) tree)

    (= (node-type tree) :split)
    (or (find-tile-by-entity (first (:children tree)) entity-id)
        (find-tile-by-entity (second (:children tree)) entity-id))))

(defn remove-entity-from-layout
  "Remove an entity from the layout tree and collapse empty splits.
   Returns nil if the entire tree becomes empty."
  [tree entity-id]
  (letfn [(remove-and-collapse [node]
            (cond
              (nil? node) nil

              (= (node-type node) :tile)
              (if (= (:entity-id node) entity-id)
                nil  ;; Remove this tile
                node)

              (= (node-type node) :split)
              (let [[c1 c2] (:children node)
                    new-c1 (remove-and-collapse c1)
                    new-c2 (remove-and-collapse c2)]
                (cond
                  (nil? new-c1) new-c2  ;; Collapse: return sibling
                  (nil? new-c2) new-c1  ;; Collapse: return sibling
                  :else (assoc node :children [new-c1 new-c2])))

              :else node))]
    (remove-and-collapse tree)))

(defn collect-entity-ids
  "Walk layout tree and return a vector of entity IDs (in order)"
  [node]
  (cond
    (nil? node) []
    (= (node-type node) :tile) (if (:entity-id node) [(:entity-id node)] [])
    (= (node-type node) :split) (into [] (mapcat collect-entity-ids (:children node)))
    :else []))

(defn update-split-ratio
  "Update the ratio of a split (clamped 0.1-0.9)"
  [tree split-id new-ratio]
  (letfn [(update-node [node]
            (cond
              (nil? node) nil

              (= (node-type node) :tile) node

              (= (node-type node) :split)
              (if (= (:id node) split-id)
                (assoc node :ratio (min 0.9 (max 0.1 new-ratio)))
                (assoc node :children
                       (mapv update-node (:children node))))))]
    (update-node tree)))

(defn append-entity
  "Append an entity to an existing layout by wrapping in a horizontal split.
   If layout is nil, creates a single tile."
  [layout entity-id tile-id split-id]
  (let [new-tile (create-tile tile-id entity-id)]
    (if (nil? layout)
      new-tile
      (create-split split-id :horizontal layout new-tile 0.5))))
