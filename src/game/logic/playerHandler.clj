(ns game.logic.playerHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]
            [game.logic.playerHandler :as player]
            [game.logic.partitionner :as part]))

(defn treat-collision-player-partitionned
  [{:keys [player e-proj-p] :as state}]
  (let [nearby-proj (part/adjacent-entities player e-proj-p)
        collided-proj (common/get-collision-data player nearby-proj)
        updated-player (common/apply-damage collided-proj)]
    (-> state
        (assoc :player updated-player))))

(defn treat-collision-player
  [{:keys [player e-proj] :as state}]
  (let [collided-proj (common/get-collision-data player e-proj)
        updated-player (common/apply-damage collided-proj)]
    (-> state
        (assoc :player updated-player))))

(defn get-shoot-data 
  [{:keys [player timestamp] :as state}]
  (let [target (common/get-target player state)]
    (if (common/can-shoot? player state)
      (let [updated-entity (e/update-timestamp player timestamp)]
        {:entity updated-entity :projectiles (e/create-projectile updated-entity target)})
      {:entity player :projectiles []})))

(defn player-shoot 
  [state]
  (let [proj-data (get-shoot-data state)
        new-proj (:projectiles proj-data)]
    (-> state
        (assoc :player (:entity proj-data))
        (assoc :new-proj new-proj))))

(defn update-angle-player 
  [{:keys [player] :as state}]
  (let [updated-player (e/update-angle player (common/get-target player state))]
    (assoc state :player updated-player)))

(defn input-to-vector 
  "generates vector based on the inputs, for the player"
  [player inputs] 
  (let [speed (:speed player)
        y (- (if (contains? inputs :down) speed 0) (if (contains? inputs :up) speed 0))
        x (- (if (contains? inputs :right) speed 0) (if (contains? inputs :left) speed 0))]
    {:vec-x x :vec-y y}))

(defn move-player 
  [{:keys [player inputs speed] :as state}]
  (let [vec (input-to-vector player inputs)
        vector (if (e/is-alive? player) vec {:vec-x 0 :vec-y 0})
        updated-player (e/move player vector speed)]
    (assoc state :player updated-player)))

(defn correct-positions 
  [{:keys [player bounds] :as state}]
  (assoc state :player (e/correct-position player bounds)))

(defn return-player-data 
  "Returns parts of the state that were treated here"
  [{:keys [player new-proj]}]
  {:player player
   :p-proj new-proj})

(defn next-tick 
  [state]
  (-> state
      (treat-collision-player-partitionned)
      ;(treat-collision-player)
      (move-player)
      (correct-positions)
      (player-shoot)
      (update-angle-player)
      (return-player-data)))