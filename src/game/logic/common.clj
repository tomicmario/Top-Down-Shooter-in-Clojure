(ns game.logic.common
  (:require [game.entity.entities :as e]))

(defn in-bounds? [entity state]
  (let [bounds (:bounds state)]
    (and (>= (:x entity) (:min-x bounds)) (>= (:y entity) (:min-y bounds))
         (<= (:x entity) (:max-x bounds)) (<= (:y entity) (:max-y bounds)))))

(defn get-collide-damage [collisions]
  (reduce + (mapv :health collisions)))

(defn extract-from-data [type data] ; util function for pairs, filters nils
  (filterv identity (mapv type data)))

(defn closer-than-distance? [a b d]
  ; Simplified version not using sqrt
  (let [dist (* d d)
        distY (* (- (:y a) (:y b)) (- (:y a) (:y b)))
        distX (* (- (:x a) (:x b)) (- (:x a) (:x b)))]
    (> dist (+ distX distY))))

(defn colliding? [a b]
  (let [max-dist (+ (/ (:width a) 2) (/ (:width b) 2))]
    (closer-than-distance? a b max-dist)))

(defmulti get-target (fn [entity & []] [(:type entity)]))

(defmethod get-target [:kamikaze] [[] state]
  (:player state))

(defmethod get-target [:shooter] [[] state]
  (:player state))

(defmethod get-target [:player] [[] state]
  (:mouse state))

(defmulti can-shoot? (fn [entity & []] [(:type entity)]))

(defmethod can-shoot? [:shooter] [entity state]
  (> (:timestamp state) (+ (:last-shot entity) (:firerate entity))))

(defn get-collision-data [entity projectiles]
  (let [collide-cond (fn [e] (colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles colliding}))

(defn apply-damage [d] ; requires the map of get-collision data
  (e/damage-entity (get-collide-damage (:projectiles d)) (:entity d)))

(defmethod can-shoot? [:kamikaze] [entity state]
  (let [player (:player state)
        shoot-distance 30]
    (and (closer-than-distance? entity player shoot-distance)
         (e/is-alive? player))))

(defmethod can-shoot? [:player] [entity state]
  ;(== 1 1))
  (and (> (:timestamp state) (+ (:last-shot entity) (:firerate entity)))
       (contains? (:inputs state) :click)
       (e/is-alive? entity)))