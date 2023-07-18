(ns game.logic.playerHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]))

(defn treat-collision-player [state]
  (let [collided-proj (common/get-collision-data (:player state) (:e-proj state))
        updated-player (common/apply-damage collided-proj)]
    (-> state
        (assoc :player updated-player))))

(defn get-shoot-data [state]
  (let [player (:player state)
        timestamp (:timestamp state)
        target (common/get-target player state)]
    (if (common/can-shoot? player state)
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
  (let [updated-player (e/update-angle (:player state) (common/get-target (:player state) state))]
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