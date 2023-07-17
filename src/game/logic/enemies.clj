(ns game.logic.enemies
  (:require [game.entities :as e]
            [game.state :as state]))

(defn extract-from-data [type data] ; util function for pairs, filters nils
  (filterv identity (mapv type data)))

(defn closer-than-distance? [a b d]
  ; Simplified version not using sqrt
  (let [dist (* d d)
        distY (* (- (:y a) (:y b)) (- (:y a) (:y b)))
        distX (* (- (:x a) (:x b)) (- (:x a) (:x b)))]
    (> dist (+ distX distY))))

(def exclusion-radius 150)

(def max-enemy 10)

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

(defn treat-collision-enemies [state]
  (let [collision-data (mapv (fn [e] (get-collision-data e (:p-proj state))) (:enemies state)) 
        updated-enemies (mapv apply-damage collision-data)]
    (-> state
        (assoc :enemies updated-enemies))))

(defmulti get-target (fn [entity & []] [(:type entity)]))

(defmethod get-target [:kamikaze] [[] state]
  (:player state))

(defmethod get-target [:shooter] [[] state]
  (:player state))

(defmulti can-shoot? (fn [entity & []] [(:type entity)]))

(defmethod can-shoot? [:shooter] [entity state]
  (> (:timestamp state) (+ (:last-shot entity) (:firerate entity))))

(defmethod can-shoot? [:kamikaze] [entity state]
  (let [player (:player state)
        shoot-distance 30]
    (and (closer-than-distance? entity player shoot-distance)
         (e/is-alive? player))))

(defn get-shoot-data [entity state]
  (let [timestamp (:timestamp state)
        target (get-target entity state)]
    (if (can-shoot? entity state)
      (let [updated-entity (e/update-timestamp entity timestamp)]
        {:entity updated-entity :projectiles (e/create-projectile updated-entity target)})
      {:entity entity :projectiles []})))

(defn enemies-shoot [state]
  (let [proj-data (mapv (fn [e] (get-shoot-data e state)) (:enemies state))
        shot-projectiles (extract-from-data :projectiles proj-data)
        updated-enemies (extract-from-data :entity proj-data)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :new-proj (into [](flatten shot-projectiles))))))


(defn correct-positions [state]
  (let [corrected-entity (fn [e] (e/correct-position e (:bounds state)))]
    (assoc state :enemies (mapv corrected-entity (:enemies state)))))

(defn move-enemies [state]
  (let [get-entity-vec (fn [e] (e/gen-vector e (get-target e state)))
        move-enemy (fn [e] (e/move e (get-entity-vec e)))
        enemies (map move-enemy (:enemies state))]
    (assoc state :enemies enemies)))

(defn clean-enemies [state]
  (let [updated-enemies (filterv e/is-alive? (:enemies state))
        killed (- (count (:enemies state)) (count updated-enemies))
        new-score (+ (:score state) killed)]
    (-> state
        (assoc :enemies updated-enemies)
        (assoc :score new-score))))

; SPAWN LOGIC
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
        close-enough? (closer-than-distance? {:x rand-x :y rand-y} player exclusion-radius)]
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

; for display purpose, calculates here at which angle the entities should be turned
(defn update-angle-enemies [state]
  (let [fn-target (fn [e] (e/calculate-angle (get-target e state) e))
        updated-enemy (fn [e] (assoc e :angle (fn-target e)))]
    (assoc state :enemies (mapv updated-enemy (:enemies state)))))

(defn return-enemy-data [state]
  {:enemies (:enemies state)
   :e-proj (:new-proj state)
   :score (:score state)})

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

(next-tick (state/get-state))