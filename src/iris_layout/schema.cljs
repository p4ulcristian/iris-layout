(ns iris-layout.schema
  "Runtime prop validation for iris-layout components.
   Logs warnings in development when props don't match expected shapes.")

(defn warn [& args]
  (.warn js/console (apply str "[iris-layout] " args)))

(defn validate-position
  "Check that a position is a vector of two integers: [x y]"
  [label pos]
  (when pos
    (when-not (and (vector? pos)
                   (= 2 (count pos))
                   (every? integer? pos))
      (warn label " should be [x y] vector of integers, got: " (pr-str pos)))))

(defn validate-workspaces
  "Check that workspaces is a map with [x y] vector keys."
  [workspaces]
  (when workspaces
    (when-not (map? workspaces)
      (warn ":workspaces should be a map, got: " (type workspaces)))
    (doseq [k (keys workspaces)]
      (when-not (and (vector? k)
                     (= 2 (count k))
                     (every? integer? k))
        (warn ":workspaces keys should be [x y] vectors of integers, got key: " (pr-str k))))))

(defn validate-entities
  "Check that entities is a map with keyword keys, each value having :id and :name."
  [entities]
  (when entities
    (when-not (map? entities)
      (warn ":entities should be a map, got: " (type entities)))
    (doseq [[k v] entities]
      (when-not (keyword? k)
        (warn ":entities keys should be keywords, got: " (pr-str k)))
      (when-not (:name v)
        (warn ":entities[" (pr-str k) "] is missing :name")))))

(defn validate-fn
  "Check that a prop is a function."
  [label f]
  (when (and f (not (fn? f)))
    (warn label " should be a function, got: " (type f))))

(defn validate-grid-props
  "Validate all props passed to grid-component."
  [{:keys [workspaces active-position active-entity entities
           render-entity-tile on-workspaces-change
           on-active-position-change on-active-entity-change
           on-entity-close on-entity-color-change]}]
  (validate-workspaces workspaces)
  (validate-position ":active-position" active-position)
  (validate-entities entities)
  (validate-fn ":render-entity-tile" render-entity-tile)
  (validate-fn ":on-workspaces-change" on-workspaces-change)
  (validate-fn ":on-active-position-change" on-active-position-change)
  (validate-fn ":on-active-entity-change" on-active-entity-change)
  (validate-fn ":on-entity-close" on-entity-close)
  (validate-fn ":on-entity-color-change" on-entity-color-change))
