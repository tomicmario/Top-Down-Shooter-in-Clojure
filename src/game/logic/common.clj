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

(defn new-target
  [player-vec]
  (let [index (int (rand (count player-vec)))
        target (get player-vec index)]
   {:x (:x target) :y (:y target) 
    :health (:health target) :index index}))

(defn default-player-target
  [{:keys [target]} 
   player-vec]
  (let [updated-target (get player-vec (:index target))]
    (if (and updated-target (e/is-alive? updated-target)) updated-target
      (new-target player-vec))))

(defmulti get-target 
  (fn [entity _] [(:type entity)]))

(defmethod get-target [:kamikaze]
  [self {:keys [player]}]
  (default-player-target self player))

(defmethod get-target [:shooter]
  [self {:keys [player]}]
  (default-player-target self player))

(defmethod get-target [:player]
  [player _]
  (if (:mouse player) (:mouse player)
    {:x 0 :y 0}))

(defmulti can-shoot?
  (fn [entity _] [(:type entity)]))

(defmethod can-shoot? [:shooter]
  [entity state]
  (> (:timestamp state) (+ (:last-shot entity) (:firerate entity))))

(defmethod can-shoot? [:kamikaze] 
  [entity state]
  (let [player (:target entity)
        shoot-distance 30]
    (and (closer-than-distance? entity player shoot-distance)
         (e/is-alive? player)
         (> (:timestamp state) (+ (:last-shot entity) (:firerate entity))))))

(defmethod can-shoot? [:player] 
  [entity state]
  (and (> (:timestamp state) (+ (:last-shot entity) (:firerate entity)))
       (contains? (:inputs entity) :click)
       (< 2 (:timestamp state)) ;temp hack
       (e/is-alive? entity)))
