(ns game.logic.playerHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]))

(defn get-damage
  [player
   {:keys [e-proj]}]
  (transduce (comp
              (filter #(common/colliding? player %))
              (map :health))
             + e-proj))

(defn input-to-vector
  "generates vector based on the inputs, for the player"
  [{:keys [speed]} 
   inputs]
  (let [y (- (if (contains? inputs :down) speed 0) (if (contains? inputs :up) speed 0))
        x (- (if (contains? inputs :right) speed 0) (if (contains? inputs :left) speed 0))]
    {:vec-x x :vec-y y}))

(defn gen-vector
  [{:keys [inputs] :as player}]
  (if (e/is-alive? player)
    (input-to-vector player inputs)
    {:vec-x 0 :vec-y 0}))

(defn update-player
  [player
   {:keys [bounds speed timestamp] :as state}]
  (let [target (common/get-target player state)]
    (-> player
        (e/damage-entity (get-damage player state))   ;apply damage
        (e/move (gen-vector player) speed)            ;move
        (assoc :target target)                        ;assign target to not use multifn
        (#(if (e/is-alive? %)
            (e/update-angle % target) %))             ;update angle for display
        (e/correct-position bounds)                   ;correct out of bounds
        (#(if (common/can-shoot? player state)        ;update timestamp as a way to signify the player can shoot
            (e/update-timestamp % timestamp) %)))))

(defn create-proj 
  [player timestamp]
  (if (== timestamp (:last-shot player))
    (e/create-projectile player (:target player)) []))

(defn shoot
  [{:keys [player] :as t_state} 
   {:keys [timestamp]}]
  (assoc! t_state :p-proj (reduce concat (mapv #(create-proj % timestamp) player))))

(defn next-tick
  [state]
  (-> (transient {})
      (assoc! :player (mapv #(update-player % state) (:player state)))
      (shoot state)
      (persistent!)))