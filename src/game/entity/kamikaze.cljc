(ns game.entity.kamikaze
  (:require [game.entity.common :as common]
            [game.entity.projectile :as proj]))

(defn move 
  [entity vector speed]
  (common/default-move entity vector speed))

(defn create 
  [x y] 
  (common/entity x y 100 20 20 0.6 :kamikaze {:last-shot -100 :firerate 0.5}))

(defn create-projectile
  [entity target]
  (let [x (:x entity)
        y (:y entity)
        vec (common/gen-vector entity target)]
    (proj/create x y 1 50 0 vec (+ (:last-shot entity) 1))))