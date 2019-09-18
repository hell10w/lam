(ns lam.input
  (:require [cljs.core.match :refer-macros [match]]
            [lam.tools :as tools]))

(defprotocol IInput
  (is-done? [this])
  (event-to-action [this event])
  (apply-action [this event action])
  (next-input [this event action])
  (next-metrics [this event action])
  (dispatch-event [this event]))

(defrecord InputState [text input metrics]
  IInput

  (is-done? [_]
    (let [[position input] input
          maximal-position (dec (count text))]
      (and (= maximal-position position)
           (not (nil? (get input maximal-position))))))

  (event-to-action [_ event]
    (let [{:keys [name code key]} event]
      (match [name code key]
             [:keydown _ "Escape"] :escape
             [:keydown _ "Backspace"] :backspace
             [:keypress _ "Enter"] :new-line
             [:keypress _ _] (when (= 1 (count key)) :key)
             :else nil)))

  (next-input [_ event action]
    (let [key (:key event)
          [position user-input] input]
      (case action
        :backspace [(tools/dec+ position)
                    user-input]
        :new-line  [(tools/inc- position (count text))
                    (assoc user-input position "\n")]
        :key       [(tools/inc- position (count text))
                    (assoc user-input position key)]
        nil)))

  (next-metrics [_ event action]
    (let [key (:key event)
          [position _] input
          {:keys [start done removes errors pressed]} metrics
          is-start? (nil? start)
          is-done? (and (nil? done) (= (inc position) (count text)))
          is-wrong? (not= key (get text position))]
      (when-let [update (case action
                          :backspace {:removes (inc removes)}
                          :key (cond-> {}
                                       is-start? (assoc :start (tools/get-timestamp))
                                       is-done? (assoc :done (tools/get-timestamp))
                                       true (assoc :pressed (inc pressed))
                                       is-wrong? (assoc :errors (inc errors)))
                          nil)]
        (merge metrics update))))

  (apply-action [this event action]
    (let [input* (next-input this event action)
          metrics* (next-metrics this event action)]

      (when (some tools/not-empty? [input* metrics*])
        (cond-> this
                metrics* (assoc :metrics metrics*)
                input* (assoc :input input*)))))

  (dispatch-event [this event]
    (let [action (event-to-action this event)]
      [action (apply-action this event action)])))

(defn new-input-state [text]
  (map->InputState
    {:text text
     :input [0 {}]
     :metrics {:start nil
               :done nil
               :removes 0
               :errors 0
               :pressed 0}}))
