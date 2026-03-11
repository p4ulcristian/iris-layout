(ns iris-demo.components.entity)

(defn entity-renderer
  "Render an entity based on its type"
  [entity]
  (when entity
    [:div.entity-content
     (case (:type entity)
       :browser
       [:div.entity-browser
        [:iframe {:src (:url entity "https://example.com")
                  :title (:name entity)}]]

       :notes
       [:div.entity-notes
        [:textarea {:placeholder "Type your notes here..."
                    :default-value ""}]]

       :terminal
       [:div.entity-terminal
        "$ "]

       [:div.entity-placeholder
        (:name entity "Empty")])]))

(defn entity-item
  "Render a draggable entity item in the sidebar"
  [entity on-drag-start]
  [:div.entity-item
   {:draggable true
    :on-drag-start #(on-drag-start entity %)}
   [:div.entity-name (:name entity)]
   [:div.entity-type (name (:type entity))]])