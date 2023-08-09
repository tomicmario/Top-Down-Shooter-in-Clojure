(ns game.entity.entities 
  (:require [game.entity.projectile :as proj])
  (:require [game.entity.player :as player])
  (:require [game.entity.kamikaze :as kamikaze])
  (:require [game.entity.shooter :as shooter])
  (:require [game.entity.common :as common]))

(defn is-alive? 
  [entity]
  (> (:health entity) 0))

(defn gen-vector 
  [entity target]
  (common/gen-vector entity target))

(defn damage-entity 
  [damage entity]
  (assoc entity :health (- (:health entity) damage)))

(defn update-timestamp 
  [entity timestamp]
  (merge entity {:last-shot timestamp}))

(defn update-angle
  [entity target]
  (let [angle (common/calculate-angle target entity)]
    (assoc entity :angle angle)))

; ENTITIES DEFINITION
(defn default-player
  [x y]
  (player/default-player x y))

(defn kamikaze
  [x y]
  (kamikaze/create x y))

(defn shooter
  [x y]
  (shooter/create x y))

(defn random-enemy
  []
  (rand-nth [kamikaze shooter]))
; END ENTITY DEFINITION

; MOVEMENT RELATED
(defn correct-position 
  [{:keys [x y] :as entity} 
   bounds]
  (let [{:keys [min-x min-y max-x max-y]} bounds
        new-x (min max-x (max min-x x))
        new-y (min max-y (max min-y y))
        new-pos {:x new-x :y new-y}]
    (common/apply-position entity new-pos)))

(defmulti move 
  (fn [entity & []] [(:type entity)]))

(defmethod move [:projectile] 
  [entity & [speed]]
  (proj/move entity speed))

(defmethod move [:player] 
  [entity vector & [speed]]
  (player/move entity vector speed))

(defmethod move [:kamikaze]
  [entity vector & [speed]]
  (kamikaze/move entity vector speed))

(defmethod move [:shooter]
  [entity vector  & [speed]]
  (shooter/move entity vector speed))
; END MOVEMENT

; PROJECTILE CREATION
(defmulti create-projectile 
  (fn [entity & []] [(:type entity)]))

(defmethod create-projectile [:player]
  [entity mousePosition]
  (player/create-projectile entity mousePosition))

(defmethod create-projectile [:shooter]
  [entity target]
  (shooter/create-projectile entity target))

(defmethod create-projectile [:kamikaze]
  [entity target]
  (kamikaze/create-projectile entity target))