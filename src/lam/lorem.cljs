(ns lam.lorem
  (:require [clojure.string :as str]))

(defprotocol ILoremIpsum
  (make-word [this])
  (make-sentence [this])
  (make-paragraph [this])
  (make-text [this n]))

(extend-type string
  ILoremIpsum

  (make-word [this]
    (let [l (+ 3 (rand-int 3))]
      (->> #(rand-nth this)
           (repeatedly)
           (take l)
           (str/join ""))))

  (make-sentence [this]
    (let [l (+ 2 (rand-int 10))]
      (->> #(make-word this)
           (repeatedly)
           (take l)
           (str/join " ")
           (#(str % "."))
           (str/capitalize))))

  (make-paragraph [this]
    (let [l (+ 2 (rand-int 10))]
      (->> #(make-sentence this)
           (repeatedly)
           (take l)
           (str/join " "))))

  (make-text [this n]
    (->> #(make-paragraph this)
         (repeatedly)
         (take n))))
