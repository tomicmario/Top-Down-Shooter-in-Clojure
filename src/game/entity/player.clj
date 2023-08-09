(ns game.entity.player
  (:require [game.entity.common :as common])
  (:require [game.entity.projectile :as proj]))

(defn move 
  [entity vector speed]
  (common/default-move entity vector speed))

(defn default-player 
  [x y]
  (common/entity x y 100 20 20 1 :player {:last-shot -100 :firerate 10}))

(defn generate-projectile 
  [x y] 
  (proj/create x y 25 10 5 nil))

(defn create-projectile 
  [entity mousePosition]
  (let [proj (generate-projectile (:x entity) (:y entity))
        vec (common/gen-vector proj mousePosition)]
    (merge proj vec)))
