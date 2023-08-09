(ns game.entity.shooter
  (:require [game.entity.common :as common])
  (:require [game.entity.projectile :as proj]))

(defn move 
  [entity vector speed]
  (common/default-move entity vector speed))

(defn create 
  [x y]
  (common/entity x y 200 30 30 0.25 :shooter {:last-shot 20 :firerate 100}))

(defn generate-projectile 
  [x y]
  (proj/create x y 15 30 1.5 nil))

(defn create-projectile 
  [entity mousePosition]
  (let [proj (generate-projectile (:x entity) (:y entity))
        vec (common/gen-vector proj mousePosition)]
    (merge proj vec)))