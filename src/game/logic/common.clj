(ns game.logic.common
  (:require [game.entity.entities :as e]))

(defn in-bounds?
  [entity
   {:keys [bounds]}]
  (and (<= (:min-x bounds) (:x entity) (:max-x bounds))
       (<= (:min-y bounds) (:y entity) (:max-y bounds))))

(defn get-collide-damage 
  [collisions]
  (reduce + (mapv :health collisions)))

(defn extract-from-data 
  "Util function for pairs, filters nils"
  [type data] 
  (filterv identity (mapv type data)))

(defn closer-than-distance? 
  "Simplified version not using sqrt"
  [a b d]
  (let [dist (* d d)
        distY (* (- (:y a) (:y b)) (- (:y a) (:y b)))
        distX (* (- (:x a) (:x b)) (- (:x a) (:x b)))]
    (> dist (+ distX distY))))

(defn colliding? 
  [a b]
  (let [max-dist (+ (/ (:width a) 2) (/ (:width b) 2))]
    (closer-than-distance? a b max-dist)))

(defmulti get-target 
  (fn [entity _] [(:type entity)]))

(defmethod get-target [:kamikaze]
  [_ state]
  (:player state))

(defmethod get-target [:shooter] 
  [_ state]
  (:player state))

(defmethod get-target [:player] 
  [_ state]
  (:mouse state))

(defn apply-damage 
  "requires the map of get-collision data"
  [d]
  (e/damage-entity (:entity d) (get-collide-damage (:projectiles d)) ))

(defmulti can-shoot?
  (fn [entity _] [(:type entity)]))

(defmethod can-shoot? [:shooter]
  [entity state]
  (> (:timestamp state) (+ (:last-shot entity) (:firerate entity))))

(defmethod can-shoot? [:kamikaze] 
  [entity state]
  (let [player (:player state)
        shoot-distance 30]
    (and (closer-than-distance? entity player shoot-distance)
         (e/is-alive? player)
         (> (:timestamp state) (+ (:last-shot entity) (:firerate entity))))))

(defmethod can-shoot? [:player] 
  [entity state]
  (and (> (:timestamp state) (+ (:last-shot entity) (:firerate entity)))
       (contains? (:inputs state) :click)
       (not (contains? (:inputs state) :reset)) ;temp hack
       (e/is-alive? entity)))
