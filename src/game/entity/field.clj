(ns game.entity.field 
  (:require [game.entity.entities :as e]))

(def bounds {:min-x 0.0 :min-y 0.0 :max-x 500.0 :max-y 500.0})

(def min-range 120)
(def max-range 150)

(def render-range {:x 150 :y 150})

(defn default-player
  []
  (let [x (/ (:max-x bounds) 2)
        y (/ (:max-y bounds) 2)]
    (e/default-player x y)))

(defn default-field
  []
  {:player [(default-player)] :p-proj [] :e-proj []
   :enemies [] :timestamp 0 :bounds bounds :score 0 :speed 1
   :render-range render-range
   :render-bounds bounds})