(ns iris-demo.core
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdc]
            [devtools.core :as devtools]
            [iris-layout.core :as iris-layout]))

(def initial-entities
  {:editor   {:id "editor"   :name "editor.cljs" :type "code"     :color "#6366f1" :icon (fn [] [:i.fa-solid.fa-code])}
   :terminal {:id "terminal" :name "Terminal"     :type "terminal" :color "#10b981" :icon (fn [] [:i.fa-solid.fa-terminal])}
   :browser  {:id "browser"  :name "Preview"      :type "browser"  :color "#f59e0b" :icon (fn [] [:i.fa-solid.fa-globe])}
   :notes    {:id "notes"    :name "Notes"         :type "notes"    :color "#ec4899" :icon (fn [] [:i.fa-solid.fa-note-sticky])}
   :repl     {:id "repl"     :name "REPL"          :type "repl"     :color "#8b5cf6" :icon (fn [] [:i.fa-solid.fa-chevron-right])}})

(def initial-workspaces
  {[0 0] {:layout {:type "split" :id "s1" :direction "horizontal" :ratio 0.6
                   :children [{:type "tile" :id "t1" :entity-id "editor"}
                               {:type "split" :id "s2" :direction "vertical" :ratio 0.6
                                :children [{:type "tile" :id "t2" :entity-id "terminal"}
                                           {:type "tile" :id "t3" :entity-id "repl"}]}]}}
   [1 0] {:layout {:type "split" :id "s3" :direction "horizontal" :ratio 0.5
                   :children [{:type "tile" :id "t4" :entity-id "editor"}
                               {:type "tile" :id "t5" :entity-id "browser"}]}}
   [0 1] {:layout {:type "tile" :id "t6" :entity-id "notes"}}
   [1 1] {:layout {:type "split" :id "s4" :direction "vertical" :ratio 0.5
                   :children [{:type "tile" :id "t7" :entity-id "terminal"}
                               {:type "tile" :id "t8" :entity-id "repl"}]}}})

(defn entity-tile [entity]
  [:div {:style {:display "flex" :align-items "center" :justify-content "center"
                 :height "100%" :width "100%"
                 :font-size "14px" :color "rgba(255,255,255,0.7)"}}
   (:name entity)])

(defonce state
  (r/atom {:workspaces      initial-workspaces
           :active-position [0 0]
           :active-entity   nil
           :entities        initial-entities}))

(defn app []
  (let [{:keys [workspaces active-position active-entity entities]} @state]
    [iris-layout/grid-component
      {:workspaces              workspaces
       :active-position         active-position
       :active-entity           active-entity
       :entities                entities
       :render-entity-tile      entity-tile
       :on-workspaces-change    #(swap! state assoc :workspaces %)
       :on-active-position-change #(swap! state assoc :active-position %)
       :on-active-entity-change #(swap! state assoc :active-entity %)
       :on-entity-color-change  (fn [entity-id color]
                                  (swap! state assoc-in [:entities (keyword entity-id) :color] color))
       :logo [:i.fa-solid.fa-eye]}]))

(defonce root (rdc/create-root (js/document.getElementById "app")))

(defn init! []
  (devtools/install!)
  (rdc/render root [app]))
(defn reload! [] (rdc/render root [app]))
