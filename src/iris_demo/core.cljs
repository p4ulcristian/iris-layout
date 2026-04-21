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
   :repl     {:id "repl"     :name "REPL"          :type "repl"     :color "#8b5cf6" :icon (fn [] [:i.fa-solid.fa-chevron-right])}
   :chat     {:id "chat"     :name "Chat"          :type "chat"     :color "#a371f7" :icon (fn [] [:i.fa-solid.fa-comments])}
   :database {:id "database" :name "Database"      :type "database" :color "#4a9"    :icon (fn [] [:i.fa-solid.fa-database])}
   :git      {:id "git"      :name "Git"           :type "git"      :color "#f78166" :icon (fn [] [:i.fa-brands.fa-git-alt])}
   :settings {:id "settings" :name "Settings"      :type "settings" :color "#8b949e" :icon (fn [] [:i.fa-solid.fa-gear])}
   :files    {:id "files"    :name "Files"          :type "files"    :color "#3b82f6" :icon (fn [] [:i.fa-solid.fa-folder])}
   :logs     {:id "logs"     :name "Logs"           :type "logs"     :color "#eab308" :icon (fn [] [:i.fa-solid.fa-list])}})

(def initial-workspaces
  {[1 1] {:layout {:type "split" :id "s1" :direction "horizontal" :ratio 0.5
                   :children [{:type "tile" :id "t1" :entity-id "logs"}
                               {:type "tile" :id "t3" :entity-id "notes"}]}}
   [2 1] {:layout {:type "split" :id "s3" :direction "horizontal" :ratio 0.5
                   :children [{:type "tile" :id "t4" :entity-id "editor"}
                               {:type "tile" :id "t5" :entity-id "browser"}]}}
   [0 1] {:layout {:type "tile" :id "t6" :entity-id "notes"}}
   [1 0] {:layout {:type "split" :id "s4" :direction "vertical" :ratio 0.5
                   :children [{:type "tile" :id "t7" :entity-id "terminal"}
                               {:type "tile" :id "t8" :entity-id "repl"}]}}})

(def lorem-lines
  (vec (for [i (range 1 60)]
         (str i ". " (nth ["Rewriting the flux capacitor in Clojure."
                             "State is just a lie your CPU tells you."
                             "Pure functions never hurt anybody."
                             "Have you tried turning the monad inside out?"
                             "The real side effects were the friends we made along the way."
                             "Immutability: because past you was an idiot."
                             "This function has O(1) opinions and O(n) regrets."
                             "Laziness is a virtue when the seq is infinite."
                             "We don't mutate state here, we just strongly suggest it changes."
                             "The garbage collector is the only one cleaning up after you."]
                            (mod i 10))))))

(r/defc entity-tile [entity]
  (let [t (:type entity)]
    (case t
      "logs"
      [:div {:style {:height "100%" :overflow-y "auto" :padding "12px"
                     :font-family "monospace" :font-size "12px"
                     :color "rgba(255,255,255,0.6)" :line-height "1.6"}}
       (for [line lorem-lines]
         ^{:key line}
         [:div {:style {:border-bottom "1px solid rgba(255,255,255,0.04)"
                        :padding "3px 0"}}
          line])]

      "notes"
      [:div {:style {:height "100%" :overflow-y "auto" :padding "16px"
                     :font-size "13px" :color "rgba(255,255,255,0.7)"
                     :line-height "1.8"}}
       (for [i (range 20)]
         ^{:key i}
         [:p {:style {:margin "0 0 12px"}}
          (nth lorem-lines i)])]

      [:div {:style {:display "flex" :align-items "center" :justify-content "center"
                     :height "100%" :width "100%"
                     :font-size "14px" :color "rgba(255,255,255,0.7)"}}
       (:name entity)])))

(defonce state
  (r/atom {:workspaces      initial-workspaces
           :active-position [1 1]
           :active-entity   nil
           :entities        initial-entities}))

(def entity-types
  (vec (map (fn [[_ e]] {:type (:type e) :name (:name e) :color (:color e) :icon (:icon e)})
            initial-entities)))

(r/defc app []
  (let [{:keys [workspaces active-position active-entity entities]} @state]
    [iris-layout/grid-component
      {:workspaces              workspaces
       :active-position         active-position
       :active-entity           active-entity
       :entities                entities
       :entity-types            entity-types
       :render-entity-tile      entity-tile
       :on-workspaces-change    #(swap! state assoc :workspaces %)
       :on-active-position-change #(swap! state assoc :active-position %)
       :on-active-entity-change #(swap! state assoc :active-entity %)
       :on-entity-color-change  (fn [entity-id color]
                                  (swap! state assoc-in [:entities (keyword entity-id) :color] color))
       :on-entity-create        (fn [{:keys [instance-id type name color]}]
                                  (let [template (get initial-entities (keyword type))]
                                    (swap! state assoc-in [:entities (keyword instance-id)]
                                           {:id instance-id :name name :type type
                                            :color (or color (:color template))
                                            :icon (:icon template)})))
       :logo [:i.fa-solid.fa-eye]}]))

(defonce root (rdc/create-root (js/document.getElementById "app")))

(defn init! []
  (devtools/install!)
  (rdc/render root [app]))
(defn reload! [] (rdc/render root [app]))
