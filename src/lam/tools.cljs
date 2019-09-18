(ns lam.tools
  (:require [clojure.string :as str]
            [rum.core :as rum]
            [lam.lorem :as l]
            [lam.data :as data]))

(def not-empty? (complement empty?))

(defn get-timestamp []
  (.getTime (js/Date.)))

(defn jsonify [value]
  (-> value
      (clj->js)
      (js/JSON.stringify nil 1)))

(defn- keyboard-wrapper [args local-state name f]
  (fn [event]
    (let [code (.-code event)
          key (.-key event)]
      (f {:name name
          :code code
          :key key}
         local-state
         args))
    nil))

(defn keyboard-mixin
  ([handler]
   (keyboard-mixin handler (constantly {}) ::key))

  ([handler state-fabric state-key]
   {:will-mount
    (fn [state]
      (let [args (:rum/args state)
            local-state (atom (apply state-fabric args))
            component (:rum/react-component state)]
        (add-watch local-state state-key
          (fn [_ _ _ _]
            (rum/request-render component)))
        (-> state
            (assoc state-key local-state)
            (assoc ::bounds
                   (->> [:keyup :keydown :keypress]
                        (map (fn [event]
                               (let [wrapped (keyboard-wrapper args local-state event handler)
                                     event (name event)]
                                 (.addEventListener js/document event wrapped)
                                 [event wrapped])))
                        (into {}))))))

    :will-unmount
    (fn [state]
      (let [bounds (::bounds state)]
        (doseq [[event handler] bounds]
          (.removeEventListener js/document event handler)))
      (assoc state ::bounds nil))}))

(defn dec+ [n]
  (if (pos? n)
    (dec n)
    n))

(defn inc- [n limit]
  (if (< n (dec limit))
    (inc n)
    n))

(defn limit-text-length [lines limit]
  (when lines
    (let [c (->> lines
                 (map count)
                 (reductions +)
                 (take-while #(<= % limit))
                 (count))
          result (take c lines)]
      (if-not (empty? result)
        result
        [(.substring (first lines) 0 limit)]))))

(defn make-limited-text
  ([lang]
   (make-limited-text lang 500))

  ([lang limit]
   (let [abc (get-in data/langs [lang :abc])]
     (as-> abc v
           (l/make-text v 20)
           (limit-text-length v limit)
           (str/join "\n\n" v)))))
