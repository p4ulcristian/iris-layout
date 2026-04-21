(ns iris-layout.mobile.component
  (:require [reagent.core :as r]
            [iris-layout.layout :as layout]
            [iris-layout.pane.tile :as tile]
            [iris-layout.command-center.core :as command-center]
            [iris-layout.mobile.gestures :as gestures]))

(defn- all-entity-ids [workspaces]
  (vec (mapcat (fn [[_ ws]] (layout/collect-entity-ids (:layout ws)))
               workspaces)))

(defn- find-active-tile [workspaces entity-id]
  (some (fn [[_ ws]] (layout/find-tile-by-entity (:layout ws) entity-id))
        workspaces))

(defn- navigate [workspaces active-entity dir on-active-entity-change]
  (let [ids (all-entity-ids workspaces)
        n   (count ids)
        idx (.indexOf ids active-entity)]
    (when (and (pos? n) on-active-entity-change)
      (on-active-entity-change
        (get ids (mod (+ idx (if (= dir :left) -1 1)) n))))))

(defn- remove-tile [workspaces tile-id]
  (into {}
        (map (fn [[pos ws]]
               [pos (update ws :layout layout/remove-tile-from-layout tile-id)])
             workspaces)))

(defn- resolve-active-entity [workspaces active-entity]
  (or active-entity (first (all-entity-ids workspaces))))

(r/defc mobile-component
  [{:keys [workspaces active-entity entities render-entity-tile
           on-active-entity-change on-entity-close on-entity-color-change
           on-workspaces-change entity-types on-entity-create
           active-position on-active-position-change logo]}]
  (r/with-let []
    (let [active-entity  (resolve-active-entity workspaces active-entity)
          node           (find-active-tile workspaces active-entity)
          swipe-handlers (gestures/use-swipe
                           (fn [dir]
                             (case dir
                               (:left :right) (navigate workspaces active-entity dir on-active-entity-change)
                               :up            (reset! command-center? true)
                               nil))
                           {})]
      [:div.iris-mobile-viewport
       (merge {} swipe-handlers)
       (when node
         [tile/entity-tile-component
          {:node                    node
           :on-close                (fn [tile-id]
                                      (when on-entity-close (on-entity-close (:entity-id node)))
                                      (when on-workspaces-change
                                        (on-workspaces-change (remove-tile workspaces tile-id))))
           :focused?                true
           :entities                entities
           :render-entity-tile      render-entity-tile
           :on-active-entity-change on-active-entity-change
           :on-entity-color-change  on-entity-color-change}])
       [command-center/command-center
        {:cols                      3
         :rows                      3
         :workspaces                workspaces
         :active-position           active-position
         :active-entity             active-entity
         :entities                  entities
         :entity-types              entity-types
         :on-active-position-change on-active-position-change
         :on-workspaces-change      on-workspaces-change
         :on-entity-create          on-entity-create
         :on-active-entity-change   on-active-entity-change
         :command-center?           (r/atom false)
         :logo                      logo}]])))
