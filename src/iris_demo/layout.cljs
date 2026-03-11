(ns iris-demo.layout)

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
    (= (:type tree) :tile)
    (when (= (:id tree) tile-id) tree)

    (= (:type tree) :split)
    (or (find-tile (first (:children tree)) tile-id)
        (find-tile (second (:children tree)) tile-id))))

(defn replace-tile
  "Replace a tile in the tree with a new node"
  [tree tile-id new-node]
  (cond
    (nil? tree) nil

    (= (:type tree) :tile)
    (if (= (:id tree) tile-id)
      new-node
      tree)

    (= (:type tree) :split)
    (assoc tree :children
           [(replace-tile (first (:children tree)) tile-id new-node)
            (replace-tile (second (:children tree)) tile-id new-node)])))

(defn split-tile
  "Insert a split at tile position with new entity"
  [tree tile-id direction new-entity-id new-tile-id split-id]
  (let [existing-tile (find-tile tree tile-id)]
    (when existing-tile
      (let [new-tile (create-tile new-tile-id new-entity-id)
            new-split (create-split split-id direction existing-tile new-tile 0.5)]
        (replace-tile tree tile-id new-split)))))

(defn can-merge?
  "Check if a split can be merged (has only one child with content)"
  [split]
  (and (= (:type split) :split)
       (let [[child1 child2] (:children split)]
         (or (nil? (:entity-id child1))
             (nil? (:entity-id child2))))))

(defn get-parent-split
  "Find the parent split of a given tile"
  [tree tile-id]
  (letfn [(find-parent [node parent]
            (cond
              (nil? node) nil

              (= (:type node) :tile)
              (when (= (:id node) tile-id) parent)

              (= (:type node) :split)
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
        (replace-tile tree (:id parent) sibling))
      tree)))

(defn update-split-ratio
  "Update the ratio of a split (clamped 0.1-0.9)"
  [tree split-id new-ratio]
  (letfn [(update-node [node]
            (cond
              (nil? node) nil

              (= (:type node) :tile) node

              (= (:type node) :split)
              (if (= (:id node) split-id)
                (assoc node :ratio (min 0.9 (max 0.1 new-ratio)))
                (assoc node :children
                       (mapv update-node (:children node))))))]
    (update-node tree)))