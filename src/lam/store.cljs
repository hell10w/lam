(ns lam.store
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.match :refer-macros [match]]
            [cljs.core.async :refer [<!]]
            [goog.functions :as gf]
            [rum.core :as rum]
            [reitit.frontend.easy :as rfe]
            [lam.data :as data]
            [lam.wiki :as wiki]))

(defonce state (atom {:route nil
                      :quotes {}}))

(defn- get-training-progress [state]
  (let [route (:route state)
        name (keyword (get-in route [:data :name]))
        {:keys [layout step]} (:path-params route)
        layout (keyword layout)
        step (int step)
        step- (dec step)
        exercises (get data/exercises-by-layouts layout)
        is-last? (= step (count exercises))
        current (get exercises step-)]
    {:name name
     :layout layout
     :step step
     :step- step-
     :exercises exercises
     :is-last? is-last?
     :current current}))

(defn- get-practice [state]
  (let [{:keys [route quotes]} state
        {:keys [lang]} (:path-params route)]
    {:lang lang
     :quotes quotes}))

(defonce training (rum/derived-atom [state] ::training-progress get-training-progress))
(defonce practice (rum/derived-atom [state] ::practice get-practice))

(defn switch-to-exercise []
  (let [{:keys [layout step]} @training]
    (rfe/push-state ::exercise {:layout layout :step step})))

(defn switch-to-next-step []
  (let [{:keys [layout step is-last?]} @training]
    (if is-last?
      (rfe/push-state ::select-lang)
      (rfe/push-state ::brief {:layout layout :step (inc step)}))))

(defn reset-quote [lang]
  (swap! state update-in [:quotes] dissoc lang))

(def load-quote
  (gf/debounce
    (fn [lang]
      (go (if-let [data (<! (wiki/get-wiki-quote lang))]
            (swap! state assoc-in [:quotes lang] data)
            (load-quote lang))))
    1000))

(defn on-navigate [new-match]
  (swap! state assoc :route new-match))
