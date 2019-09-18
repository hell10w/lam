(ns lam.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [cljs.core.match :refer-macros [match]]
            [cljs.core.async :refer [<!]]
            [rum.core :as rum]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]
            [lam.input :as inp]
            [lam.data :as data]
            [lam.store :as store]
            [lam.tools :as tools]))

(enable-console-print!)

(rum/defc Index []
  [:.mw7.center
   (conj [:div] data/intro)])

(rum/defc SelectLayout []
  [:.mw7.pa2.center
   [:h2 "Выберите раскладку:"]
   [:div
    [:.center
     (for [l data/layouts]
       [:.pv2 {:key l}
        [:a {:href (rfe/href ::store/brief {:layout l :step 1})}
         (name l)]])]]])

(rum/defc SelectLang []
  [:.mw7.pa2.center
   [:h2 "Выберите язык:"]
   [:div
    [:.center
     (for [[lang {:keys [title abc]}] data/langs]
       [:.pv2 {:key lang}
        [:a {:href (rfe/href ::store/select-source {:lang lang})}
         title]
        [:br]
        abc])]]])

(rum/defc SelectSource
  < rum/reactive
  []
  (let [{:keys [lang]} (rum/react store/practice)]
    [:.mw7.pa2.center
     [:div
      [:.center
       (for [[name title] [[::store/practice-with-wiki "Викицитаты"]
                           [::store/practice-with-random "Случайный набор символов"]]]
         [:.pv2 {:key name}
          [:a {:href (rfe/href name {:lang lang})}
           title]])]]]))

(rum/defc Json [data]
  [:pre.overflow-y-scroll
   (tools/jsonify data)])

(rum/defc PrettyPrint [data]
  [:pre.overflow-y-scroll
   (with-out-str (pp/pprint data))])

(rum/defc Letter
  < rum/static
  [cursor? letter user-letter]
  (let [correct? (= letter user-letter)
        incorrect? (not (or correct? (nil? user-letter)))
        [letter invisible? new-line?] (case letter
                                        "\n" ["⏎" true true]
                                        " " ["␣" true false]
                                        [letter false false])]
    (if-not new-line?
      [:span
       {:class [(when cursor? "outline")
                (when correct? "bg-washed-green")
                (when incorrect? "bg-washed-red")
                (when invisible? "black-40")]}
       letter]
      [:span
       {:class [(when cursor? "outline")
                (when correct? "bg-washed-green")
                (when incorrect? "bg-washed-red")
                (when invisible? "black-40")]}
       letter
       [:br]])))

(def CachedLetter (memoize (fn [cursor? letter user-letter]
                             (Letter cursor? letter user-letter))))

(rum/defc TextInputContent
  < rum/static
  [text position input show-cursor]
  (into [:.code {:style {:word-break "break-all"}}]
        (map (fn [[index letter]]
               (CachedLetter (and show-cursor
                                  (= position index))
                             letter
                             (get input index)))
             (map-indexed vector text))))

(rum/defcs TextInput
  < rum/static
    (tools/keyboard-mixin
      (fn [event local-state [text opts]]
        (let [{:keys [escape done] :or {escape #(inp/new-input-state text)
                                        done (constantly nil)}} opts
              [action new-state] (inp/dispatch-event @local-state event)]
          (when new-state
            (let [is-done? (inp/is-done? new-state)]
              (reset! local-state new-state)
              (when (and done is-done?)
                (done new-state))))
          (when (= action :escape)
            (if escape
              (escape)
              (reset! local-state (inp/new-input-state text))))))
      #(inp/new-input-state %)
      ::keyboard-state)

  [state text _]
  (let [{[position input] :input} @(::keyboard-state state)]
    (TextInputContent text position input true)))

(rum/defc Metrics
  < rum/state
    (tools/keyboard-mixin
      (fn [event _ [_ _ close]]
        (let [{:keys [name code key]} event]
          (match [name code key]
                 [:keydown _ "Escape"] (close)
                 [:keypress _ "Enter"] (close)
                 [:keypress _ " "] (close)
                 :else nil))))
  [text metrics close]
  (let [{:keys [start done errors pressed]} metrics
        elapsed (/ (- done start) 1000)
        per-minute (* 60 (/ (count text) elapsed))
        errors% (* 100 (/ errors (count text)))]
    [:div
     [:table.mv3.pa0.center
      [:tbody
       [:tr
        [:td "Скорость"]
        [:td [:b (.toFixed per-minute 0)] " символов в минуту"]]
       [:tr
        [:td "Заняло"]
        [:td [:b (.toFixed elapsed 3)] " сек."]]
       [:tr
        [:td "Нажато"]
        [:td [:b pressed] " клавиш"]]
       [:tr
         [:td "Ошибок"]
         [:td [:b errors] " (" [:b (.toFixed errors% 1) "%"] ")"]]]]
     [:.tc
      [:a.link.dim.pointer {:tab-index -1 :on-click close}
       [:span.lh-solid.underline "Далее"]
       [:span.pl2 "⏎"]]]]))

(rum/defcs TextInputWithMetrics
  < rum/static
    (rum/local nil ::final)
  [state text opts]
  (let [final (::final state)]
    (if @final
      (let [{:keys [input text]} @final
            [position input] input]
        [:div
         (TextInputContent text position input false)
         (Metrics text (:metrics @final) (:done opts))])
      (let [opts* (assoc opts :done #(reset! final %))]
        (TextInput text opts*)))))

(rum/defcs RandomPractice
  < rum/reactive
    (rum/local nil ::text)
    {:after-render (fn [state]
                     (let [text (::text state)
                           {:keys [lang]} @store/practice]
                       (when-not @text
                         (reset! text (tools/make-limited-text lang))))
                     state)}
  [state]
  (let [local (::text state)
        text @local]
    (if-not text
      "Генерация"
      [:.mw7.center.pa3.bg-white.shadow-1
       (TextInputWithMetrics text {:escape #(reset! local nil)
                                   :done #(reset! local nil)})])))

(rum/defc Quote
  < {:did-mount (fn [state]
                  (let [[lang quote] (:rum/args state)]
                    (when-not quote
                      (store/load-quote lang)))
                  state)
     :did-remount (fn [_ state]
                    (let [[lang quote] (:rum/args state)]
                      (when-not quote
                        (store/load-quote lang)))
                    state)}
  [lang quote]
  (if-not quote
    "Загрузка"
    (let [{:keys [title url lines]} quote
          text (str/join "\n" lines)]
      [:div.mv4.ph4.pv4.bg-white.shadow-1
       [:div.mb4
        [:h1.di
         [:a.black {:tab-index -1 :target "blank" :href url}
          title]]]
       (TextInputWithMetrics text {:escape #(store/reset-quote lang)
                                   :done #(store/reset-quote lang)})])))

(rum/defc WikiPractice
  < rum/reactive
  []
  (let [state (rum/react store/practice)
        {:keys [lang quotes]} state
        quote (get quotes lang)]
    [:.mw7.center
     (Quote lang quote)]))

(rum/defc UnknownTraining []
  [:.mw7.center
   [:h1.black-50.mv4
    [:i.fas.fa-exclamation-triangle.f3]
    [:span.pa2 "Неверная раскладка или номер шага"]]
   [:.mv4
    [:a.lh-solid.link.dim.pointer {:tab-index -1
                                   :on-click #(rfe/replace-state ::training)}
     [:span.v-top.fl "←"]
     [:span.pa1.underline "Вернуться к выбору раскладки"]]]])

(rum/defc Brief
  < rum/reactive
    (tools/keyboard-mixin
      (fn [{:keys [name code key]} _ _]
        (match [name code key]
               [:keypress _ "Enter"] (store/switch-to-exercise)
               :else nil)))

  []
  (let [{:keys [current]} (rum/react store/training)
        inner (:brief current)]
    (if-not current
      (UnknownTraining)
      (-> (reduce conj [:.ph5.pv4.mv3.f5.lh-copy.w-100.bg-white.shadow-1] inner)
          (conj [:.tc [:a.link.dim.pointer {:tab-index -1
                                            :on-click store/switch-to-exercise}
                       [:span.lh-solid.underline "Далее"]
                       [:span.pl2 "⏎"]]])))))

(rum/defc KeysReminder [hint]
  (when hint
    [:table.collapse.center.bg-white.shadow-1
     [:tbody
      (for [letter hint
            :let [[side title] (get data/hints letter)]]
        [:tr.striped--light-gray
         [:td.ph2.tc (when (= side :l) letter)]
         [:td.ph2.tc (when (= side :r) letter)]
         [:td.ph2 title]])]]))

(rum/defc Exercise
  < rum/reactive
  []
  (let [{:keys [current]} (rum/react store/training)
        {:keys [hint text]} current]
    (if-not current
      (UnknownTraining)
      [:div
       (when hint
         (KeysReminder hint))
       [:.pa4.mv3.bg-white.shadow-1
        (TextInputWithMetrics (str/join "\n" text)
                              {:done store/switch-to-next-step})]])))

(rum/defc ProgressBar
  < rum/reactive
  []
  (let [{:keys [layout step exercises current]} (rum/react store/training)]
    (when current
      [:ul.progress-indicator
       {:style {:margin-bottom 0}}
       (for [index (range (count exercises))
             :let [index+ (inc index)]]
         [:li
          {:key index
           :class [(when (not= index+ step) "pointer")
                   (when (< index+ step) "completed")
                   (when (= index+ step) "active")]
           :on-click #(rfe/push-state ::store/brief {:layout layout :step index+})}
          [:span.bubble]
          index+])])))

(rum/defc Header []
  [:.pa3.mb4.bg-white.shadow-1
   [:.mw7.center
    [:.flex.items-center
     [:a.mr3.no-underline {:tab-index -1
                           :href (rfe/href ::store/index)}
      [:i.fas.fa-keyboard.f3]]
     [:a.ph3 {:tab-index -1 :href (rfe/href ::store/select-layout)} "Разучивание клавиатуры"]
     [:a.ph3 {:tab-index -1 :href (rfe/href ::store/select-lang)} "Практика"]]]])

(rum/defc App
  < rum/reactive
  []
  (let [data (get-in (rum/react store/state) [:route :data])
        {:keys [show-progress-bar component]} data]
    [:.min-vh-100.flex.flex-column.justify-between.helvetica
     [:div
      (Header)
      (when show-progress-bar
        [:.mw7.center
         [:.pa3.w-100
          (ProgressBar)]])]
     [:.mw7.center
      (if-not component
        [:h1 "Страница не найдена"]
        (component))]
     [:.mt4.pa2.bg-white.shadow-4
      [:.mw7.center.black-50.f7
       [:.flex.flex-row.items-center.justify-center
        [:i.pa2
         [:a.black {:href "https://github.com/xoreaxebx/"
                    :target "blank"
                    :tab-index -1}
          [:i.ph1.f5.fab.fa-github.v-mid]]]]]]]))

(def routes
  [["/" {:name ::store/index :component Index}]

   ["/training"
    ["" {:name ::store/select-layout :component SelectLayout}]

    ["/:layout/:step" {:show-progress-bar true}
     ["" {:name ::store/brief :component Brief}]
     ["/exercise" {:name ::store/exercise :component Exercise}]]]

   ["/practice"
    ["" {:name ::store/select-lang :component SelectLang}]
    ["/:lang"
     ["" {:name ::store/select-source :component SelectSource}]
     ["/wiki" {:name ::store/practice-with-wiki :component WikiPractice}]
     ["/random" {:name ::store/practice-with-random :component RandomPractice}]]]])

(defn init! []
  (rfe/start!
    (rf/router routes)
    store/on-navigate
    {:use-fragment true})
  (rum/mount (App) (.getElementById js/document "app")))

(init!)

;(defn on-js-reload [& args])
