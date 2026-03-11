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
  "Insert a split at tile position with new entity"
  [tree tile-id direction new-entity-id new-tile-id split-id]
  (let [existing-tile (find-tile tree tile-id)]
    (when existing-tile
      (let [new-tile (create-tile new-tile-id new-entity-id)
            new-split (create-split split-id direction existing-tile new-tile 0.5)]
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
