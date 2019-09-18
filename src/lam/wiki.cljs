(ns lam.wiki
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :as str]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [lam.tools :as tools]))

(defn- wikiquote-api [lang query-params]
  (http/get (str "https://" lang ".wikiquote.org/w/api.php")
            {:with-credentials? false
             :query-params (merge {:origin "*"
                                   :action "query"
                                   :format "json"}
                                  query-params)}))

(defn- first-non-empty [& texts]
  (first (remove empty? texts)))

(defn- get-content-by-selector [doc selector]
  (remove empty?
          (map #(str/trim (first-non-empty (.-textContent %) (.-innerText %) ""))
               (array-seq (.querySelectorAll doc selector)))))

(defn- clean-wiki-html [content]
  (let [doc (.parseFromString (js/DOMParser.) content "text/html")]
    (first-non-empty (get-content-by-selector doc "ul > li")
                     (get-content-by-selector doc "table tr:first-of-type td:nth-child(2)"))))

(defn get-wiki-quote
  ([]
   (get-wiki-quote "en"))

  ([lang]
   (go (when-let [title (some-> (wikiquote-api lang
                                               {:list "random"
                                                :rnnamespace 0
                                                :rnlimit 1})
                                (<!)
                                (get-in [:body :query :random 0 :title]))]
         (when-let [content (some-> (wikiquote-api lang
                                                   {:action "parse"
                                                    :page title
                                                    :prop "text"
                                                    :section "1"
                                                    :disablelimitreport 1
                                                    :disabletoc 1})
                                    (<!)
                                    (get-in [:body :parse :text :*]))]
           (when-let [lines (-> content
                                (clean-wiki-html)
                                (tools/limit-text-length 500))]
             {:url (str "https://" lang ".wikiquote.org/wiki/" title)
              :title title
              :lines lines}))))))
              ;:content content}))))))
