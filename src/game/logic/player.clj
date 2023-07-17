(ns game.logic.player
  (:require [game.entities :as e]
            [game.state :as state]
            [game.logic.player :as player]))

(defn closer-than-distance? [a b d]
  ; Simplified version not using sqrt
  (let [dist (* d d)
        distY (* (- (:y a) (:y b)) (- (:y a) (:y b)))
        distX (* (- (:x a) (:x b)) (- (:x a) (:x b)))]
    (> dist (+ distX distY))))

(defn colliding? [a b]
  (let [max-dist (+ (/ (:width a) 2) (/ (:width b) 2))]
    (closer-than-distance? a b max-dist)))

(defn get-collide-damage [collisions]
  (reduce + (mapv :health collisions)))

(defn apply-damage [d] ; requires the map of get-collision data
  (e/damage-entity (get-collide-damage (:projectiles d)) (:entity d)))

(defn get-collision-data [entity projectiles]
  (let [collide-cond (fn [e] (colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles colliding}))

(defn treat-collision-player [state]
  (let [collided-proj (get-collision-data (:player state) (:e-proj state))
        updated-player (apply-damage collided-proj)]
    (-> state
        (assoc :player updated-player))))

(defn get-target [state]
  (:mouse state))

(defn can-shoot? [entity state]
  ;(== 1 1))
  (and (> (:timestamp state) (+ (:last-shot entity) (:firerate entity)))
       (contains? (:inputs state) :click) 
       (e/is-alive? entity)))

(defn get-shoot-data [state]
  (let [player (:player state)
        timestamp (:timestamp state)
        target (get-target state)]
    (if (can-shoot? player state)
      (let [updated-entity (e/update-timestamp player timestamp)]
        {:entity updated-entity :projectiles [(e/create-projectile updated-entity target)]})
      {:entity player :projectiles []})))

(defn player-shoot [state]
  (let [proj-data (get-shoot-data state)
        new-proj (:projectiles proj-data)]
    (-> state
        (assoc :player (:entity proj-data))
        (assoc :new-proj new-proj))))

(defn update-angle-player [state]
  (let [angle (e/calculate-angle (:mouse state) (:player state))
        updated-player (assoc (:player state) :angle angle)]
    (assoc state :player updated-player)))

(defn input-to-vector [player inputs] ; generates vector based on the inputs, for the player
  (let [speed (:speed player)
        y (- (if (contains? inputs :down) speed 0) (if (contains? inputs :up) speed 0))
        x (- (if (contains? inputs :right) speed 0) (if (contains? inputs :left) speed 0))]
    {:vec-x x :vec-y y}))

(defn move-player [state]
  (let [player (:player state)
        vec (input-to-vector player (:inputs state))
        vector (if (e/is-alive? player) vec {:vec-x 0 :vec-y 0})
        updated-player (e/move player vector)]
    (assoc state :player updated-player)))

(defn correct-positions [state]
  (assoc state :player (e/correct-position (:player state) (:bounds state))))

(defn return-player-data [state]
  {:player (:player state) 
   :p-proj (:new-proj state)})

(defn next-tick [state]
  (-> state
      (treat-collision-player)
      (move-player)
      (correct-positions)
      (player-shoot)
      (update-angle-player)
      (return-player-data)))

(next-tick (state/get-state))
