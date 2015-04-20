(ns asols.client
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [sablono.core :refer-macros [html]]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >! chan close!]]
            [goog.string :as gstring]
            [goog.string.format]
            [figwheel.client :as fw]
            [asols.commands :refer [TrainOpts MutationOpts] :as commands])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn- str->keyword
  [value]
  (keyword (apply str (rest value))))

(defonce app-state
  (atom {:connection nil
         :running?   false
         :settings   {:train-opts    (TrainOpts. 0.3 0.9 5E-4 1000)
                      :mutation-opts (MutationOpts. nil nil 5 true true)
                      :hidden-layer-choices []
                      :out-layer-choices []}
         :solvings   []}))

(defn- checkbox
  "Simple checkbox which binds its value to path in cursor"
  [cursor path label-text]
  {:pre [(om/cursor? cursor)
         (coll? path)
         (string? label-text)]}
  [:.form-group
   [:.col-sm-12
    [:label.checkbox
     [:input.custom-checkbox
      {:type      "checkbox"
       :checked   (when (get-in cursor path) "checked")
       :on-change #(om/update! cursor path (.. % -target -checked))}]
     [:span.icons
      [:span.icon-checked]
      [:span.icon-unchecked]]
     label-text]]])

(defn- input
  ([cursor path]
    (input cursor path identity))
  ([cursor path clean-fn]
   [:input.form-control
    {:value     (get-in cursor path)
     :on-change #(let [value (.. % -target -value)
                       cleaned-value (clean-fn value)
                       final-value (if (js/isNaN cleaned-value)
                                     value
                                     cleaned-value)]
                  (om/update! cursor path final-value))}]))

(defcomponent settings-panel [{:keys [start-chan running? settings]} owner]
  (render [_]
    (let [[label-width field-width] [4 8]
          label-class (str "col-sm-" label-width)
          input-class (str "col-sm-" field-width)]
      (html
       [:.panel.panel-default.settings
        [:.panel-heading "Settings"]
        [:.panel-body
         [:.row
          [:.col-sm-6
           [:form.form-horizontal
            [:.form-group
             [:label.control-label {:class label-class} "Learning rate"]
              [:div {:class input-class}
               (input settings [:train-opts :learning-rate] js/parseFloat)]]
            [:.form-group
             [:label.control-label {:class label-class} "Momentum"]
             [:div {:class input-class}
              (input settings [:train-opts :momentum] js/parseFloat)]]
            [:.form-group
             [:label.control-label {:class label-class} "Weight decay"]
             [:div {:class input-class}
              (input settings [:train-opts :weight-decay] js/parseFloat)]]
            [:.form-group
             [:label.control-label {:class label-class} "Iterations"]
             [:div {:class input-class}
              (input settings [:train-opts :iter-count] js/parseInt)]]
            [:.form-group
             [:div {:class (str input-class " col-sm-offset-" label-width)}
              [:button.btn.btn-primary.btn-block
               {:type     "button"
                :disabled (when running? "disabled")
                :on-click #(go (>! start-chan {}))}
               "Start"]]]]]
          [:.col-sm-6
           [:form.form-horizontal
            [:.form-group
             [:label.control-label {:class label-class} "Hidden layer"]
             [:div {:class input-class}
              [:select.form-control
               {:on-change #(->> (.. % -target -value)
                                 (str->keyword)
                                 (om/update! settings [:mutation-opts :hidden-layer-type]))
                :value     (:hidden-layer-type (:train-opts settings))}
               (for [choice (:hidden-layer-choices settings)]
                 [:option {:value choice} (name choice)])]]]

            [:.form-group
             [:label.control-label {:class label-class} "Output layer"]
             [:div {:class input-class}
              [:select.form-control
               {:value (:out-layer-type (:train-opts settings))
                :on-change #(->> (.. % -target -value)
                                 (str->keyword)
                                 (om/update! settings [:mutation-opts :out-layer-type]))}
               (for [choice (:out-layer-choices settings)]
                 [:option {:value choice} (name choice)])]]]

            [:.form-group
             [:label.control-label {:class label-class} "Repeat times"]
             [:div {:class input-class}
              (input settings [:mutation-opts :repeat-times] js/parseInt)]]

            (checkbox settings [:mutation-opts :remove-edges?]
                      "Remove edges?")
            (checkbox settings [:mutation-opts :remove-nodes?]
                      "Remove nodes?")]]]]]))))

(defmulti mutation-view :operation)

(defmethod mutation-view :asols.mutations/identity [m]
  [:p "Nothing"])

(defmethod mutation-view :asols.mutations/add-neuron [m]
  [:p "Added node "
   [:span.label.label-primary (name (:added-neuron m))]])

(defmethod mutation-view :asols.mutations/add-edge [m]
  (let [[node-from node-to] (:added-edge m)]
    [:p "Added edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :asols.mutations/del-neuron [m]
  [:p "Removed node "
   [:span.label.label-primary (name (:deleted-neuron m))]])

(defmethod mutation-view :asols.mutations/del-edge [m]
  (let [[node-from node-to] (:deleted-edge m)]
    [:p "Removed edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :asols.mutations/add-layer [m]
  [:p (gstring/format "Added hidden layer at %s " (:layer-index m))
   [:span.label.label-info (name (:layer-type m))]])

(defn format-error [error]
  (gstring/format "%.5f" error))

(defn format-variance [variance]
  (gstring/format "%.5f" variance))

(defn format-time [ms-took]
  (condp > ms-took
    1E3 (str (int ms-took) " ms")
    1E6 (gstring/format "%.2f sec" (/ ms-took 1E3))))

(defcomponent solving-case-block [{:keys [solving-case hover-chan best?]}]
  (render [_]
    (let [{:keys [mean-error variance]} solving-case]
      (html
        [:tr
         {:class         (when best? "success")
          :on-mouse-over #(go (>! hover-chan (:number @solving-case)))
          :on-mouse-out  #(go (>! hover-chan :none))}
         [:td (mutation-view (:mutation solving-case))]
         [:td (format-error mean-error)]
         [:td (format-variance variance)]]))))

(defcomponent solving-block [{:keys [solving]} owner]
  (init-state [_]
    {:visible? false
     :selected-case-num :none
     :hover-chan (chan)})

  (will-mount [_]
    (let [hover-chan (om/get-state owner :hover-chan)]
      (go-loop [selected-case-num (<! hover-chan)]
        (om/set-state! owner :selected-case-num selected-case-num)
        (recur (<! hover-chan)))))

  (render-state [_ {:keys [visible? selected-case-num hover-chan]}]
    (html
      (let [{:keys [ms-took cases]} solving
            best-case (first (sort-by :mean-error cases))
            preview-case (if (= selected-case-num :none)
                           best-case
                           (first (filter #(= (:number %) selected-case-num) cases)))]
        [:li.list-group-item.solving
         [:.row
          [:.col-sm-7
           [:p {:on-click #(om/update-state! owner :visible? not)}
            (mutation-view (:mutation best-case))]]
          [:.col-sm-5.stats
           [:span.label.label-info (format-variance (:variance best-case))]
           [:span.label.label-danger (format-error (:mean-error best-case))]
           [:span.label.label-default (format-time ms-took)]]]
         [:.row {:class (when-not visible? "hidden")}
          [:.col-sm-5
           {:dangerouslySetInnerHTML {:__html (:graph preview-case)}}]
          [:.col-sm-7
           [:table.table.table-condensed.table-hover
            [:thead [:tr [:th "Operation"] [:th "Error"] [:th "Variance"]]]
            [:tbody
             (for [{err :mean-error :as case} cases]
               (om/build solving-case-block
                         {:solving-case case
                          :hover-chan hover-chan
                          :best? (= err (:mean-error best-case))}))]]]]]))))

(defcomponent solvings-panel [{:keys [solvings]}]
  (render [_]
    (html
      [:.solvings
       [:.panel.panel-default
        [:.panel-heading "Mutations"]
        [:ul.list-group
         (for [i (range (count solvings))]
           (om/build solving-block
                     {:number  (inc i) :solving (nth solvings i)}
                     {:react-key i}))]]])))

(defcomponent app [{:keys [connection settings running? solvings] :as cursor} owner]
  (init-state [_]
    {:start-chan (chan)})

  (will-mount [_]
    (go-loop []
             (<! (om/get-state owner :start-chan))
             (om/update! cursor :running? true)
             (om/update! cursor :solvings [])
             (let [{:keys [train-opts mutation-opts]} @settings]
               (>! connection (commands/start train-opts mutation-opts)))
             (recur))

    (go-loop [frame (<! connection)]
             (if-not (nil? frame)
               (let [{message :message} frame]
                 (.debug js/console (str "Received command:" (:command message)))
                 (case (:command message)
                   ::commands/init (let [{opts :opts} message
                                       hidden-type (first (:hidden-layer-choices opts))
                                       out-type (first (:out-layer-choices opts))]
                                   (om/transact! settings #(merge % opts))
                                   (om/update! settings [:mutation-opts :hidden-layer-type] hidden-type)
                                   (om/update! settings [:mutation-opts :out-layer-type] out-type))
                   ::commands/step (let [{:keys [solving]} message]
                                   (om/transact! solvings #(conj % solving)))
                   ::commands/finished (om/update! cursor :running? false))
                 (recur (<! connection))))))
  (render-state [_ {:keys [start-chan]}]
    (html
      [:.container.app
       [:.row-fluid
        [:.col-md-12
         (om/build settings-panel {:start-chan start-chan
                                   :running? running?
                                   :settings settings})]]
       [:.row-fluid
        [:.col-md-12
         (om/build solvings-panel {:solvings solvings})]]])
                ))

(defn- start []
  (.debug js/console "Starting app")
  (go (let [ws-chan (ws-ch "ws://localhost:8000/ws" {:format :transit-json})
            {connection :ws-channel error :error} (<! ws-chan)]
        (if error
          (do
            (.error js/console "Failed to connect:")
            (.error js/console error))
          (do
            (.log js/console "Connected to websocket channel")
            (swap! app-state assoc :connection connection)
            (om/root app app-state {:target (.-body js/document)}))))))

(defn- shutdown []
  (.debug js/console "Shutdown....")
  (om/detach-root (.-body js/document))
  (when-let [conn (:connection @app-state)]
    (close! conn)))

(enable-console-print!)
(start)
(fw/start {:websocket-url "ws://localhost:3449/figwheel-ws"
           :on-jsload #(do (shutdown) (start))})
