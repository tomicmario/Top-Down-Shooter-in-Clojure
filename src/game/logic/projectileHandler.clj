(ns game.logic.projectileHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]
            [game.logic.partitionner :as part]))

(defn proj-valid? 
  [entity state]
  (let [ttl (:max-ttl entity)]
    (if (nil? ttl) true (< (:timestamp state) ttl))))

(defn clean-projectiles 
  [{:keys [p-proj e-proj] :as state}]
  (let [is-valid? (fn [p] (and (proj-valid? p state) (common/in-bounds? p state)))]
    (-> state
        (assoc :p-proj (filterv is-valid? p-proj))
        (assoc :e-proj (filterv is-valid? e-proj)))))

(defn colliding? 
  [a b]
  (let [max-dist (+ (/ (:width a) 2) (/ (:width b) 2))]
    (common/closer-than-distance? a b max-dist)))

(defn get-collision-data 
  [entity projectiles]
  (let [collide-cond (fn [e] (colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles (if (empty? colliding) [] colliding)}))

(defn remove-collided 
  "requires the map of get-collision data on collided"
  [projectiles collided] 
  (let [colliding-proj (set collided)
        has-not-collided? (fn [p] (not (contains? colliding-proj p)))]
    (filterv has-not-collided? projectiles)))

(defn treat-collision-player-partitionned 
  [{:keys [player e-proj-p e-proj] :as state}]
  (let [close-proj (part/adjacent-entities player e-proj-p)
        collided-proj (get-collision-data player close-proj)
        new-proj (remove-collided e-proj (:projectiles collided-proj))]
    (assoc state :e-proj new-proj)))

(defn treat-collision-enemies-partitionned
  [{:keys [p-proj p-proj-p enemies] :as state}]
  (let [near (fn [e] (part/adjacent-entities e p-proj-p))
        collision-data (mapv (fn [e] (get-collision-data e (near e))) enemies)
        collided-proj  (flatten (common/extract-from-data :projectiles collision-data))
        new-proj (remove-collided p-proj collided-proj)]
    (assoc state :p-proj new-proj)))

(defn treat-collision-player
  [{:keys [player e-proj] :as state}]
  (let [collided-proj (get-collision-data player e-proj)
        new-proj (remove-collided e-proj (:projectiles collided-proj))]
    (assoc state :e-proj new-proj)))

(defn treat-collision-enemies
  [{:keys [p-proj enemies] :as state}]
  (let [collision-data (mapv (fn [e] (get-collision-data e p-proj)) enemies)
        collided-proj  (flatten (common/extract-from-data :projectiles collision-data))
        new-proj (remove-collided p-proj collided-proj)]
    (assoc state :p-proj new-proj)))

(defn move-proj 
  [{:keys [speed p-proj e-proj] :as state}]
  (let [fn-move (fn [e] (e/move e speed))
        updated-e-proj (mapv fn-move e-proj)
        updated-p-proj (mapv fn-move p-proj)]
    (-> state
        (assoc :e-proj updated-e-proj)
        (assoc :p-proj updated-p-proj))))

(defn return-projectiles 
  "Returns parts of the state that were treated here"
  [{:keys [e-proj p-proj]}]
  {:p-proj p-proj
   :e-proj e-proj})

(defn next-tick 
  [state]
  (-> state
      (clean-projectiles)
      (treat-collision-enemies-partitionned)
      (treat-collision-player-partitionned)
      ;(treat-collision-enemies)
      ;(treat-collision-player)
      (move-proj)
      (return-projectiles)))