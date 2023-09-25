(ns game.logic.common
  (:require [game.entity.entities :as e]))

(defn in-bounds?
  [entity
   {:keys [bounds]}]
  (and (<= (:min-x bounds) (:x entity) (:max-x bounds))
       (<= (:min-y bounds) (:y entity) (:max-y bounds))))

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
  [_ {:keys [player]}]
  {:x (:x player) :y (:y player)})

(defmethod get-target [:shooter]
  [_ {:keys [player]}]
  {:x (:x player) :y (:y player)})

(defmethod get-target [:player] 
  [_ state]
  (:mouse state))

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
       (< 2 (:timestamp state)) ;temp hack
       (e/is-alive? entity)))
