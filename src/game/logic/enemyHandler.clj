(ns game.logic.enemyHandler
  (:require [game.entity.entities :as e]
            [game.logic.common :as common]))

(def exclusion-radius 150)

(def max-enemy 10)

(defn get-collision-data [entity projectiles]
  (let [collide-cond (fn [e] (common/colliding? entity e))
        colliding (filterv collide-cond projectiles)]
    {:entity entity :projectiles colliding}))

(defn treat-collision-enemies [state]
  (let [collision-data (mapv (fn [e] (get-collision-data e (:p-proj state))) (:enemies state)) 
        updated-enemies (mapv common/apply-damage collision-data)]
    (-> state
        (assoc :enemies updated-enemies))))

(defn get-shoot-data [entity state]
  (let [timestamp (:timestamp state)
        target (common/get-target entity state)]
    (if (common/can-shoot? entity state)
      (let [updated-entity (e/update-timestamp entity timestamp)]
        {:entity updated-entity :projectiles (e/create-projectile updated-entity target)})
      {:entity entity :projectiles []})))

(defn enemies-shoot [state]
  (let [proj-data (mapv (fn [e] (get-shoot-data e state)) (:enemies state))
        shot-projectiles (common/extract-from-data :projectiles proj-data)
        updated-enemies (common/extract-from-data :entity proj-data)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :new-proj (into [](flatten shot-projectiles))))))

(defn correct-positions [state]
  (let [corrected-entity (fn [e] (e/correct-position e (:bounds state)))]
    (assoc state :enemies (mapv corrected-entity (:enemies state)))))

(defn move-enemies [state]
  (let [get-entity-vec (fn [e] (e/gen-vector e (common/get-target e state)))
        move-enemy (fn [e] (e/move e (get-entity-vec e) (:speed state)))
        enemies (map move-enemy (:enemies state))]
    (assoc state :enemies enemies)))

(defn clean-enemies [state]
  (let [updated-enemies (filterv e/is-alive? (:enemies state))
        killed (- (count (:enemies state)) (count updated-enemies))
        new-score (+ (:score state) killed)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :score new-score))))

(defn spawn-coordinates [bounds player exclusion]
  (let [diameter (* exclusion 2)
        x (rand (- (:max-x bounds) diameter))
        y (rand (- (:max-y bounds) diameter))
        new-x (if (> (- (:x player) exclusion) x) x
                  (+ x diameter))
        new-y (if (> (- (:y player) exclusion) y) y
                  (+ y diameter))]
    {:x new-x :y new-y}))

(defn rand-coordinates [state]
  ; returns a coordinate not present withing a given amount of units from the player
  (let [bounds (:bounds state)
        player (:player state)
        rand-x (rand (:max-x bounds))
        rand-y (rand (:max-y bounds))
        close-enough? (common/closer-than-distance? {:x rand-x :y rand-y} player exclusion-radius)]
    (if close-enough? (spawn-coordinates bounds player exclusion-radius)
        {:x rand-x :y rand-y})))

(defn add-enemy [state]
  (if (< (count (:enemies state)) max-enemy)
    (let [en-fn (e/random-enemy) ; enemy create function
          rand-cor (rand-coordinates state)
          enemy (en-fn (:x rand-cor) (:y rand-cor))
          enemies (conj (:enemies state) enemy)]
      (assoc state :enemies enemies))
    state))

(defn return-enemy-data [state]
  {:enemies (:enemies state)
   :e-proj (:new-proj state)
   :score (:score state)})

(defn update-angle-enemies [state]
  (let [updated-enemy (fn [e] (e/update-angle e (common/get-target e state)))]
    (assoc state :enemies (mapv updated-enemy (:enemies state)))))

(defn next-tick [state]
  (-> state
      (treat-collision-enemies)
      (clean-enemies)
      (move-enemies)
      (correct-positions)
      (enemies-shoot)
      (add-enemy)
      (update-angle-enemies)
      (return-enemy-data)))