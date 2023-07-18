(ns game.logic.controller
  (:require [game.logic.projectileHandler :as proj]
            [game.logic.playerHandler :as player]
            [game.logic.enemyHandler :as enemies]
            [game.state :as state]))

(defn unify-data [state proj-data player-data enemy-data]
  (let [p-proj (concat (:p-proj proj-data) (:p-proj player-data))
        e-proj (concat (:e-proj proj-data) (:e-proj enemy-data))]
    (-> state
        (assoc :player (:player player-data))
        (assoc :enemies (:enemies enemy-data))
        (assoc :score (:score enemy-data))
        (assoc :p-proj p-proj)
        (assoc :e-proj e-proj))))

(defn generate-next-tick[state]
  (let [proj-data (future (proj/next-tick state))
        player-data (future (player/next-tick state))
        enemy-data (future (enemies/next-tick state))]
    (unify-data state (deref proj-data) (deref player-data) (deref enemy-data))))

(defn init-scene[min-x min-y max-x max-y]
  (let [new-bounds {:min-x min-x :min-y min-y
                    :max-x max-x :max-y max-y}]
    (reset! state/bounds new-bounds)))

; ENTIRE FRAME LOGIC
(defn next-tick []
  (-> (state/get-state)
      (generate-next-tick)
      (state/update-state)))

(next-tick)