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

(defn- parse-float
  [string]
  (if (= string "0.")
    string
    (js/parseFloat string)))

;; App state management

(defonce app-state
  (atom {:connection nil
         :running?   false
         :progress   nil
         :settings   {:train-opts    (TrainOpts. 0.3 0.9 5E-4 1000)
                      :mutation-opts (MutationOpts. nil nil true true true)
                      :hidden-choices []
                      :out-choices []}
         :solvings   []
         :failed-solving nil}))

(defn init [app hidden-choices out-choices]
  (-> app
      (assoc-in [:settings :hidden-choices] hidden-choices)
      (assoc-in [:settings :out-choices] out-choices)
      (assoc-in [:settings :mutation-opts :hidden-type] (first hidden-choices))
      (assoc-in [:settings :mutation-opts :out-type] (first out-choices))))

(defn start [app]
  (let [{:keys [train-opts mutation-opts]} (:settings app)
        start-cmd (commands/start train-opts mutation-opts)]
    (go (>! (:connection app) start-cmd)))
  (assoc app :running? true
             :solvings []
             :failed-solving nil
             :progress nil))

(defn update-progress [app mutation progress-value]
  (assoc app  :progress {:mutation mutation
                         :value progress-value}))

(defn new-solving [app solving]
  (update-in app [:solvings] conj solving))

(defn finish [app failed-solving]
  (assoc app :failed-solving failed-solving
             :running? false))

;; Widgets

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

(defcomponent progress-bar [{:keys [value]}]
  (render [_]
    (html
      (let [percents (-> (int (* value 100))
                         (min 100)
                         (max 0))]
        [:.progress
         [:.progress-bar {:role "progressbar"
                          :aria-value-now percents
                          :aria-value-min 0
                          :aria-value-max 100
                          :style {:width (str percents "%")}}
          (gstring/format "%d%% complete" percents)]]))))

(defcomponent settings-panel [{:keys [start-chan running? settings]}]
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
               (input settings [:train-opts :learning-rate] parse-float)]]
            [:.form-group
             [:label.control-label {:class label-class} "Momentum"]
             [:div {:class input-class}
              (input settings [:train-opts :momentum] parse-float)]]
            [:.form-group
             [:label.control-label {:class label-class} "Weight decay"]
             [:div {:class input-class}
              (input settings [:train-opts :weight-decay] parse-float)]]
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
                                 (om/update! settings [:mutation-opts :hidden-type]))
                :value     (:hidden-type (:train-opts settings))}
               (for [choice (:hidden-choices settings)]
                 [:option {:value choice} (name choice)])]]]

            [:.form-group
             [:label.control-label {:class label-class} "Output layer"]
             [:div {:class input-class}
              [:select.form-control
               {:value (:out-type (:train-opts settings))
                :on-change #(->> (.. % -target -value)
                                 (str->keyword)
                                 (om/update! settings [:mutation-opts :out-type]))}
               (for [choice (:out-choices settings)]
                 [:option {:value choice} (name choice)])]]]

            (checkbox settings [:mutation-opts :remove-edges?] "Remove edges?")
            (checkbox settings [:mutation-opts :remove-nodes?] "Remove nodes?")
            (checkbox settings [:mutation-opts :add-layers?] "Add layers?")]]]]]))))

(defmulti mutation-view :operation)

(defmethod mutation-view :asols.mutations/identity [_]
  [:span "nothing"])

(defmethod mutation-view :asols.mutations/add-neuron [m]
  [:span "added node "
   [:span.label.label-primary (name (:added-neuron m))]])

(defmethod mutation-view :asols.mutations/add-edge [m]
  (let [[node-from node-to] (:added-edge m)]
    [:span "added edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :asols.mutations/del-neuron [m]
  [:span "removed node "
   [:span.label.label-primary (name (:deleted-neuron m))]])

(defmethod mutation-view :asols.mutations/del-edge [m]
  (let [[node-from node-to] (:deleted-edge m)]
    [:span "removed edge "
     [:span.label.label-success (gstring/format "%s -> %s" node-from node-to)]]))

(defmethod mutation-view :asols.mutations/add-layer [m]
  [:span (gstring/format "added hidden layer at %s " (:layer-index m))
   [:span.label.label-info (name (:layer-type m))]])

(defn format-error [error]
  (gstring/format "%.5f" error))

(defn format-time [ms-took]
  (condp > ms-took
    1E3 (str (int ms-took) " ms")
    1E6 (gstring/format "%.2f sec" (/ ms-took 1E3))))

(defcomponent solving-case-block [{:keys [solving-case hover-chan case-id best?]}]
  (render [_]
    (let [{:keys [train-error test-error]} solving-case]
      (html
        [:tr
         {:class         (when best? "success")
          :on-mouse-over #(go (>! hover-chan case-id))
          :on-mouse-out  #(go (>! hover-chan :none))}
         [:td (mutation-view (:mutation solving-case))]
         [:td (format-error train-error)]
         [:td (format-error test-error)]]))))

(defcomponent solving-block [{:keys [number solving visible?]
                              :or {visible? false}} owner]
  (init-state [_]
    {:visible? visible?
     :selected-case-id :none
     :hover-chan (chan)})

  (will-mount [_]
    (let [hover-chan (om/get-state owner :hover-chan)]
      (go
        (loop [selected-case-num (<! hover-chan)]
          (om/set-state! owner :selected-case-id selected-case-num)
          (recur (<! hover-chan))))))

  (render-state [_ {:keys [visible? selected-case-id hover-chan]}]
    (html
      (let [{:keys [cases best-case ms-took]} solving
            preview-case (if (= selected-case-id :none)
                           best-case
                           (nth cases selected-case-id))]
        [:li.list-group-item.solving
         [:.row {:on-click #(om/update-state! owner :visible? not)}
          [:.col-xs-7
           [:span
            (when number (gstring/format "%d. " number))
            (mutation-view (:mutation best-case))]]
          [:.col-xs-5.stats
           [:span.label.label-danger
            "Test " (format-error (:test-error best-case))]
           [:span.label.label-warning
            "Train " (format-error (:train-error best-case))]
           [:span.label.label-default (format-time ms-took)]]]
         [:.row {:class (when-not visible? "hidden")}
          [:.col-xs-5
           {:dangerouslySetInnerHTML {:__html (:graph preview-case)}}]
          [:.col-xs-7
           [:table.table.table-condensed.table-hover
            [:thead [:tr [:th "Operation"] [:th "Train error"] [:th "Test error"]]]
            [:tbody
             (om/build solving-case-block {:solving-case best-case
                                           :hover-chan hover-chan
                                           :case-id :none
                                           :best? true})
             (for [i (range (count cases))
                   :let [case (nth cases i)]]
               (om/build solving-case-block {:solving-case case
                                             :hover-chan hover-chan
                                             :case-id i
                                             :best? false}))]]]]]))))

(defcomponent solvings-panel [{:keys [solvings]}]
  (render [_]
    (html
      [:.solvings
       [:.panel.panel-primary
        [:.panel-heading "Mutations"]
        [:ul.list-group
         (for [i (range (count solvings))]
           (om/build solving-block
                     {:number  (inc i) :solving (nth solvings i)}
                     {:react-key i}))]]])))

(defcomponent failed-solving-panel [solving]
  (render [_]
    (html
      [:.panel.panel-info
       [:.panel-heading "Further tryings:"]
       [:ul.list-group
        (om/build solving-block {:solving solving :visible? true})]])))

(defcomponent stats-panel [{:keys [running? progress]}]
  (render [_]
    (html
      [:.panel.panel-success
       [:.panel-heading "Stats"]
       (when (and running? progress)
         [:.panel-body
          [:.row-fluid
           [:.col-sm-12
            [:p "Current mutation: " [:b (mutation-view (:mutation progress))]]
            (om/build progress-bar {:value (:value progress)})]]])])))

(defcomponent app [{:keys [connection settings running? solvings] :as cursor} owner]
  (init-state [_]
    {:start-chan (chan)})

  (will-mount [_]
    (go
      (loop []
        (<! (om/get-state owner :start-chan))
        (om/transact! cursor start)
        (recur)))
    (go
      (loop [frame (<! connection)]
        (if-not (nil? frame)
          (let [{message :message} frame]
            (.debug js/console (str "Received command:" (:command message)))
            (case (:command message)
              ::commands/init
              (om/transact! cursor #(init % (:hidden-choices message) (:out-choices message)))

              ::commands/progress
              (om/transact! cursor #(update-progress % (:mutation message) (:value message)))

              ::commands/step
              (om/transact! cursor #(new-solving % (:solving message)))

              ::commands/finished
              (om/transact! cursor #(finish % (:solving message))))
            (recur (<! connection)))))))
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
         (om/build solvings-panel {:solvings solvings})]]

       (when (:failed-solving cursor)
         [:.row-fluid
          [:.col-md-12
           (om/build failed-solving-panel (:failed-solving cursor))]])

       [:.row-fluid
        [:.col-md-12
         (om/build stats-panel cursor)]]])))

(defn- launch []
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
(launch)
(fw/start {:websocket-url "ws://localhost:3449/figwheel-ws"
           :on-jsload #(do (shutdown) (launch))})
